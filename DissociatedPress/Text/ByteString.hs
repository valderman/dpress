{-# LANGUAGE OverloadedStrings #-}
-- | Less general functions for working with textual data.
module DissociatedPress.Text.ByteString (
    Word,
    randomSentence, ask, updateDictFromFile, words', unwords', insertText
  ) where
import Data.Char (toLower)
import System.Random
import DissociatedPress.Core
import qualified Data.ByteString.Lazy.Char8 as B
import qualified Data.ByteString.Lazy.UTF8 as BU

type Word = B.ByteString

-- | Definitions for words' and unwords'
whitespace, punctuation, ignore, nonword :: B.ByteString
whitespace  = " \n\t"
punctuation = ",.!?:;*/&="
ignore      = "\r()\""
nonword     = B.concat [whitespace, punctuation]

newtype WordsState = WS Int deriving Eq

stSpace, stPunct, stWord :: WordsState
stSpace = WS 0
stPunct = WS 1
stWord  = WS 2

-- | "Better" words; recognizes punctuation as separate words.
words' :: B.ByteString -> [Word]
words' = go stSpace . B.filter (not . flip B.elem ignore)
  where
    go st s
      | B.null s = []
      | st == stSpace =
        let (_, s') = B.span (flip B.elem whitespace) s
            st'     = if B.elem (B.head s') punctuation
                         then stPunct
                         else stWord
        in go st' s'
      | st == stWord =
        let (w, s') = B.span (not . flip B.elem nonword) s
            st'     = if B.elem (B.head s') punctuation
                         then stPunct
                         else stSpace
        in w:go st' s'
      | st == stPunct =
        let (w, s') = B.span (flip B.elem punctuation) s
            st'     = if B.elem (B.head s') whitespace
                         then stSpace
                         else stWord
        in w:go st' s'

-- | "Better" unwords; doesn't put spaces before punctuation.
unwords' :: [Word] -> B.ByteString
unwords' = B.concat . unwords'' where
  unwords'' (a:b:ws)
    | B.elem (B.head b) punctuation = a:unwords'' (b:ws)
    | otherwise                     = a:unwords'' ((' ' `B.cons` b):ws)
  unwords'' (w:[])                  = [w]
  unwords'' []                      = []

-- | Generates a completely random sentence
randomSentence :: Dictionary Word -> StdGen -> B.ByteString
randomSentence dic gen = ask (unwords' $ randomKey dic gen) dic gen

-- | Generate a sentence forward and backward from the given key.
--   We also filter out any punctuation from the keys, as punctuation is too
--   common to be in any good key.
ask :: B.ByteString -> Dictionary Word -> StdGen -> B.ByteString
ask key dic g =
  if B.null sentence
     then randomSentence dic g
     else sentence
  where
    takeUntil pred words =
      let (a, b) = span (not . pred) words in a ++ take 1 b
    
    -- lowercase key then split into words
    lowercaseKey  = words' $ BU.fromString $ map toLower $ BU.toString key
    
    -- we don't want any punctuation in our key; that might steal focus
    -- from important words
    nopunctuation =
      filter (\x -> not $ B.elem (B.head x) punctuation) lowercaseKey
    
    -- to generate the actual key, word split the given key then let the key
    -- generator do its magic.
    key' = toKey nopunctuation dic g
    
    -- generate forward from key
    forward  = takeUntil (flip B.elem ".!?" . B.head) $ disPress key' dic g
    
    -- generate backward from key
    backward = takeWhile (not . flip B.elem ".!?" . B.head)
                   $ disPressBack key' dic g
    
    -- generate forward and backward, concatenate and then make string
    sentence = unwords' $  reverse (drop (length key') backward)
                        ++ key' ++ (drop (length key') forward)

-- | Updates a dictionary from a file; all words are lowercased.
updateDictFromFile :: FilePath -> Dictionary Word -> IO (Dictionary Word)
updateDictFromFile seed oldAssocs =
  B.readFile seed >>= \str -> return $ insertText str oldAssocs

-- | Insert text into dictionary; all text is lowercased before insertion.
insertText :: B.ByteString -> Dictionary Word -> Dictionary Word
insertText str = updateDict (words' $ BU.fromString
                                    $ map toLower
                                    $ BU.toString str)
