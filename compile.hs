{-# LANGUAGE OverloadedStrings #-}
module Main where
import DissociatedPress.Core
import DissociatedPress.Storage
import DissociatedPress.Text as DT
import Codec.Compression.Zlib
import Data.Binary
import qualified Data.ByteString.Lazy as BSL
import qualified Data.Text as T
import Data.Text.Binary ()
import Data.Text.Encoding
import Data.Char (toLower)
import Data.List
import System.Environment (getArgs)
import System.IO

main = do
  args <- getArgs
  hPutStrLn stderr $  "Usage: input text on stdin, writes dictionary to "
                   ++ "stdout.\n-p n or -pn sets preferred key length to n.\n"
                   ++ "-k n or -kn sets max key length to n."
  let dict = readOpts args (defDict :: Dictionary DT.Word)
  hPutStrLn stderr $  "Compiling using -k" ++ show (maxKeyLen dict)
                   ++ " -p" ++ show (preferKeyLen dict) ++ "."
                   ++ if maxKeyLen dict == maxKeyLen
                                           (defDict :: Dictionary DT.Word)
                         && preferKeyLen dict == preferKeyLen
                                           (defDict :: Dictionary DT.Word)
                         then " (defaults)"
                         else ""
  BSL.interact $ compress
               . encode
               . flip insertText dict
               . decodeUtf8
               . BSL.toStrict
  where
    readOpts (a:as) d
      | take 2 a == "-k" =
        case length a of
          2 -> readOpts (drop 1 as) d {maxKeyLen = read $ head as}
          _ -> readOpts as d {maxKeyLen = read $ drop 2 a}
      | take 2 a == "-p" =
        case length a of
          2 -> readOpts (drop 1 as) d {preferKeyLen = read $ head as}
          _ -> readOpts as d {preferKeyLen = read $ drop 2 a}
    readOpts _ d = d
