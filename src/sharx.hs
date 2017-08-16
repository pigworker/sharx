{-# LANGUAGE OverloadedStrings #-}

module Main where

import System.Process
import System.Environment
import System.Directory
import Data.ByteString.Lazy as B (ByteString, concat, getContents, toStrict)
import Data.PHPSession
import Data.Text as T
import Data.Text.Encoding
import Data.Text.IO as TIO
import Data.List as L

import Raw
import Interpret

postText :: PHPSessionValue -> [(Text, Text)]
postText (PHPSessionValueArray kvs) =
  [ (decodeUtf8 (B.toStrict k), decodeUtf8 (B.toStrict v))
  | (PHPSessionValueString k, PHPSessionValueString v) <- kvs
  ]
postText _ = []

main :: IO ()
main = do
  xs <- getArgs
  pbs <- B.getContents
  let mpost = L.unfoldr decodePartialPHPSessionValue pbs
  let (inputs, getty) = case mpost of
        (post : gett : _) -> (postText post, postText gett)
        _ -> ([], [])
  case xs of
    (u : p : _) -> TIO.putStr =<< process p
    _ -> TIO.putStr "# sharx says\n I don't know what you're on about."