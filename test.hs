{-# LANGUAGE OverloadedStrings #-}
module Main where
import DissociatedPress
import System.Random
import System.IO
import System.Environment (getArgs)
import Data.Text.Binary ()
import qualified Data.Text as T

main = do
  a <- getArgs
  if null a then error "Give the name of the dictionary on the command line!"
            else return ()
  d <- load (head a) >>= return . (\(Just d) -> d)
  g <- newStdGen
  putStrLn $  "Conversation with dictionary " ++ (head a) ++ "\n"
           ++ "Max key length is " ++ show (maxKeyLen d) ++ " and "
           ++ "preferred key length is " ++ show (preferKeyLen d)
  putStr "> "
  ss <- getContents
  askSession d g (lines ss)
 where
  askSession d g (s:ss) = do
    let s' = T.pack s
    putStrLn $ T.unpack $ ask s' d g
    hFlush stdout
    putStr "> "
    hFlush stdout
    let d' = insertText s' d
    askSession d' (snd (random g :: (Bool, StdGen))) ss
  askSession _ _ _ = putStrLn ""
