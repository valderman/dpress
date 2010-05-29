{-# LANGUAGE OverloadedStrings #-}
module Main where
import DissociatedPress.Core
import DissociatedPress.Storage
import DissociatedPress.Text.ByteString as DTB
import Codec.Compression.Zlib
import Data.Binary
import qualified Data.ByteString.Lazy.Char8 as B
import qualified Data.ByteString.Lazy.UTF8 as BU
import Data.Char (toLower)
import System.Environment (getArgs)
import System.IO

main = do
  args <- getArgs
  hPutStrLn stderr $  "Usage: input text on stdin, writes dictionary to "
                   ++ "stdout.\n-p n or -pn sets preferred key length to n.\n"
                   ++ "-k n or -kn sets max key length to n.\n"
                   ++ "Defaults are -k3 -p2.\n"
  let dict = readOpts args (defDict :: Dictionary DTB.Word)
  B.interact $ compress . encode . flip updateDict dict . words'
                        . BU.fromString . map toLower . BU.toString
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
