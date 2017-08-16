{-# LANGUAGE DeriveFunctor, DeriveFoldable, DeriveTraversable, OverloadedStrings #-}

module Interpret where

import Control.Applicative
import Control.Monad
import Data.Text as T
import Data.List as L
import Data.Text.IO as TIO

import Raw

data Bwd x
  = B0
  | Bwd x :< x
  deriving (Show, Eq, Functor, Foldable, Traversable)

(<>>) :: Bwd x -> [x] -> [x]
B0 <>> ys = ys
(xs :< x) <>> ys = xs <>> (x : ys)

data Val
  = VA Text
  | Val :&& Val
  | VVoid
  | VX Text
  | VF Env [[Text]] [Clause]
  | VT Comp
  | VK Tnok
  deriving (Show, Eq)

data Comp
  = Ret {ret :: Val}
  | (Text, [Val]) :-> Tnok
  deriving (Show, Eq)

type Env  = [(Text, Val)]
type Clos = (Env, Exp)
type Kont = [([Text], Frame)]
type Tnok = Bwd ([Text], Frame)

data Hole = Hole deriving (Show, Eq)

data Frame
  = Car Hole Clos
  | Cdr Val Hole
  | Fun Hole [Clos]
  | Arg Val (Bwd Comp) Hole [([Text], Clos)]
  | Seq Hole Clos
  | Ble (Bwd Text) Hole (Blether Clos)
  deriving (Show, Eq)

run :: Clos -> Kont -> Comp
run (g, V x)     k = case lookup x g of
  Just v  -> kont v k
  Nothing -> comm ("error", [VX x]) B0 k
run (g, A x)      k = kont (VA x) k
run (g, Void)     k = kont VVoid k
run (g, a :& d)   k = run (g, a) (push (Car Hole (g, d)) k)
run (g, f :$ as)  k = run (g, f) (push (Fun Hole (fmap ((,) g) as)) k)
run (g, e1 :- e2) k = run (g, e1) (push (Seq Hole (g, e2)) k)
run (g, F cs)     k = kont (VF g (handlings (fmap fst cs)) cs) k
run (g, T be)     k = ble B0 (fmap ((,) g) be) k
{- run _             k = comm ("error", [VX "not implemented"]) B0 k -}

kont :: Val -> Kont -> Comp
kont v k = case pop k of
  Nothing -> Ret v
  Just (Car Hole c,       k)  -> run c (push (Cdr v Hole) k)
  Just (Cdr u Hole,       k)  -> kont (u :&& v) k
  Just (Fun Hole cs,      k)  -> arg v B0 (L.zip (hans v) cs) k
  Just (Arg u ts Hole cs, k)  -> arg u (ts :< Ret v) cs k
  Just (Seq Hole c,       k)  -> run c k
  Just (Ble sz Hole bc,   k)  -> ble (snarf sz v) bc k

ble :: Bwd Text -> Blether Clos -> Kont -> Comp
ble sz (s :<: Nil) k = kont (VX (T.concat (sz <>> [s]))) k
ble sz (s :<: c :>: bc) k = run c (push (Ble (sz :< s) Hole bc) k)

snarf :: Bwd Text -> Val -> Bwd Text
snarf sz (VX s) = sz :< s
snarf sz (VA s) = sz :< s
snarf sz (a :&& d) = snarf (snarf sz a) d
snarf sz _ = sz

hans :: Val -> [[Text]]
hans (VF _ hs _) = hs ++ repeat []
hans _ = repeat []

arg :: Val -> Bwd Comp -> [([Text], Clos)] -> Kont -> Comp
arg v tz []             k = app v (tz <>> []) k
arg v tz ((hs, c) : js) k = run c (hpush hs (Arg v tz Hole js) k)

app :: Val -> [Comp] -> Kont -> Comp
app (VA h)          ts k = comm (h, fmap ret ts) B0 k
app (VF g _ ls)     ts k = clauses g ls ts k
app (VT (Ret v))    [] k = kont v k
app (VT (q :-> k')) [] k = comm q k' k
app (VK k')    [Ret v] k = kont v (k' <>> k)
app _                _ k = comm ("error", [VX "applied nonsense"]) B0 k

clauses :: Env -> [Clause] -> [Comp] -> Kont -> Comp
clauses g [] _ k = comm ("abort", []) B0 k
clauses g ((ps, e) : ls) ts k = case zim match g ps ts of
  Nothing -> clauses g ls ts k
  Just g  -> run (g, e) k

zim :: (Env -> p -> v -> Maybe Env) -> Env -> [p] -> [v] -> Maybe Env
zim m g [] [] = return g
zim m g (p : ps) (v : vs) = do
  g <- m g p v
  zim m g ps vs
zim _ _ _ _ = Nothing

match :: Env -> Pat -> Comp -> Maybe Env
match g (VP p) (Ret v) = vmatch g p v
match g (CP j ps x) ((j', vs) :-> k) = do
  guard (j == j')
  g <- zim vmatch g ps vs
  return ((x, VK k) : g)
match g (TP x) t = return ((x, VT t) : g)
match _ _ _ = Nothing

vmatch :: Env -> VPat -> Val -> Maybe Env
vmatch g (PV x) v = return ((x, v) : g)
vmatch g (PQ x) v = do
  u <- lookup x g
  guard (u == v)
  return g
vmatch g (PA x) (VA y) = do
  guard (x == y)
  return g
vmatch g PVoid VVoid = return g
vmatch g (pa :&: pd) (va :&& vd) = do
  g <- vmatch g pa va
  vmatch g pd vd
vmatch _ _ _ = Nothing

comm :: (Text, [Val]) -> Tnok -> Kont -> Comp
comm q@(h, _) k ((hs, f) : l)
  | elem h hs = handle (q :-> k) f l
comm q k (l : m) = comm q (k :< l) m
comm q k [] = q :-> k

handle :: Comp -> Frame -> Kont -> Comp
handle t (Arg u tz Hole cs) k = arg u (tz :< t) cs k
handle _ _ _ = error "only Arg frames handle"

push :: Frame -> Kont -> Kont
push = hpush []

hpush :: [Text] -> Frame -> Kont -> Kont
hpush hs f k = (hs, f) : k

pop :: Kont -> Maybe (Frame, Kont)
pop ((_, f) : k) = Just (f, k)
pop _ = Nothing

handlings :: [[Pat]] -> [[Text]]
handlings = fmap (foldMap ha) . L.transpose where
  ha (CP c _ _) = [c]
  ha _          = []

execute :: ([Defn], Exp) -> Text
execute (ds, e) = case run (g', e) [] of
    Ret (VX t)   -> t
    Ret _        -> "nontextual return"
    (c, vs) :-> _ -> T.concat["unhandled ",c,pack (show vs)]
  where
    g' = mkG [] (sort ds)
    mkG g [] = g
    mkG g ((Being x :=: e) : ds) = case run (g, e) [] of
      Ret v -> mkG ((x, v) : g) ds
      _     -> mkG g ds
    mkG g ds@((Doing f _ :=: _) : _) =
      mkG ((f, mkF ls) : g) ds' where
      (ls, ds') = go ds
      go ((Doing f' ps :=: e) : ds) | f == f'
        = ((ps, e) : ls, ds') where (ls, ds') = go ds
      go ds = ([], ds)
    mkF ls = VF g'(handlings (fmap fst ls)) ls

process :: FilePath -> IO Text
process x = do
  s <- TIO.readFile x
  return $ either pack execute (grok s)