{-# LANGUAGE OverloadedStrings,
    DeriveFunctor, DeriveFoldable, DeriveTraversable #-}

module Raw where

import Control.Applicative
import Control.Monad
import Data.Text as T
import Data.Char
import Data.Attoparsec.Text as P
import Data.List as L
import Data.List.Split

grok :: Text -> Either String ([Defn], Exp)
grok s = (,) <$> parseOnly progP p
             <*> (T <$> parseOnly (bletherP expP) t)
  where (t, p) = defence s

isFence :: Text -> Bool
isFence t = case T.stripPrefix "~~~" t of
  Nothing -> False
  Just t -> T.all (\ c -> isSpace c || c == '~') t

defence :: Text -> (Text, Text)
defence t = (T.unlines xs, T.unlines ys) where
  (xs, ys) = go (splitWhen isFence (T.lines t))
  go [] = ([], [])
  go (x : ts) = (x ++ xs, ys) where (ys, xs) = go ts



data Blether x = Text :<: Whether x
  deriving (Show, Eq, Functor, Foldable, Traversable)
data Whether x = Nil | x :>: Blether x
  deriving (Show, Eq, Functor, Foldable, Traversable)
infixr 4 :<:
infixr 4 :>:

data Exp
  = V Text               -- var
  | A Text               -- atom
  | Exp :& Exp           -- cons [a | d]
  | Void                 -- []
  | Exp :$ [Exp]         -- f(e1,..,en)
  | T (Blether Exp)      -- `blah blah blah`
  | Exp :- Exp           -- e1; e2
  | F [Clause]           -- {p..p -> e | .. }
  deriving (Show, Eq)

type Clause = ([Pat], Exp)

data Pat
  = VP VPat
  | CP Text [VPat] Text
  | TP Text
  deriving (Show, Eq)

data VPat
  = PV Text
  | PQ Text
  | PA Text
  | VPat :&: VPat
  | PVoid
  | PT (Blether VPat)
  deriving (Show, Eq)

bletherP :: Parser x -> Parser (Blether x)
bletherP p = do
  s <- P.takeTill (`elem` ['{','\\','`'])
  ((s :<:) <$>
     ((:>:) <$ char '{' <*> p <* char '}' <*> bletherP p)
   <|> (do char '\\'
           c <- escape <$> anyChar
           (t :<: w) <- bletherP p
           return (append s (cons c t) :<: w))
   <|> pure (s :<: Nil))

escape :: Char -> Char
escape 'n' = '\n'
escape 'r' = '\r'
escape 'b' = '\b'
escape 't' = '\t'
escape c = c

atomP :: Parser Text
atomP = char '\'' *> P.takeWhile isAlphaNum

idenP :: Parser Text
idenP = do
  c <- peekChar'
  guard $ isAlpha c
  P.takeWhile isAlphaNum

glomP :: Parser a -> (a -> Parser a) -> Parser a
glomP p k = do
  a <- p
  glomP (k a) k <|> pure a

sP :: Parser a -> Parser a
sP p = id <$ skipSpace <*> p <* skipSpace

parP :: Parser a -> Parser [a]
parP p = id <$ char '(' <*> P.sepBy (sP p) (char ',')
            <* skipSpace <* char ')'

expP :: Parser Exp
expP = sP (glomP weeP moreP)

weeP :: Parser Exp
weeP = A <$> atomP
   <|> V <$> idenP
   <|> T <$ char '`' <*> bletherP expP <* char '`'
   <|> id <$ char '[' <*> cdrP expP Void (:&)
   <|> Void <$ char '[' <* skipSpace <* char ']'
   <|> F <$ char '{' <*> P.sepBy clauseP (char '|')
         <* skipSpace <* char '}'

cdrP :: Parser x -> x -> (x -> x -> x) -> Parser x
cdrP p v c
    =  v <$ char ']'
   <|> id <$ char '|' <*> p <* char ']'
   <|> c <$> p <*> cdrP p v c

moreP :: Exp -> Parser Exp
moreP h = (h :$) <$> parP expP
      <|> (h :-) <$ char ';' <*> expP

patP :: Parser Pat
patP = TP <$ char '<' <*> sP idenP <* char '>'
   <|> CP <$ char '<' <* skipSpace <*> atomP
          <*> parP vpatP <* sP (string "->")
          <*> idenP <* skipSpace <* char '>'
   <|> VP <$> vpatP

vpatP :: Parser VPat
vpatP = PA <$> atomP
    <|> PV <$> idenP
    <|> PQ <$ char '=' <*> idenP
    <|> id <$ char '[' <*> cdrP (sP vpatP) PVoid (:&:)
    <|> PT <$ char '`' <*> bletherP (sP vpatP) <* char '`'

clauseP :: Parser Clause
clauseP = (,) <$>
  (id <$> P.sepBy (sP patP) (char ',') <* sP (string "->")
   <|> pure [])
  <*> expP

data Defn
  = Lhs :=: Exp
  deriving (Show, Eq)

defnP :: Parser Defn
defnP = (:=:) <$> lhsP <* sP (char '=') <*> expP

data Lhs
  = Being Text
  | Doing Text [Pat]
  deriving (Show, Eq)

instance Ord Lhs where
  compare (Being _)   (Being _)   = EQ
  compare (Being _)   (Doing _ _) = GT
  compare (Doing _ _) (Being _)   = LT
  compare (Doing f _) (Doing g _) = compare f g

instance Ord Defn where
  compare (l :=: _) (m :=: _) = compare l m

lhsP :: Parser Lhs
lhsP = Doing <$> idenP <*> parP patP
   <|> Being <$> idenP

progP :: Parser [Defn]
progP = P.many' (sP defnP)
