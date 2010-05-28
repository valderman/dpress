{-# LANGUAGE OverloadedStrings #-}
module DissociatedPress.Text (
    Word,
    randomSentence, ask, updateTextDict, words', unwords'
  ) where
import Data.Char (toLower)
import System.Random
import DissociatedPress.Core
import qualified Data.ByteString.Lazy.Char8 as B

type Word = B.ByteString

-- | Definitions for words' and unwords'
whitespace, punctuation, ignore, nonword :: B.ByteString
whitespace  = " \n\t"
punctuation = ",.!?:;+-*/&="
ignore      = "\r()\""
nonword     = B.concat [whitespace,  punctuation]

newtype WordsState = WS Int deriving Eq

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
    | B.elem (B.head b) punctuation =
      a:unwords'' (b:ws)
    | otherwise =
      a:unwords'' ((' ' `B.cons` b):ws)
  unwords'' (w:[]) =
    [w]
  unwords'' [] =
    []

-- | Generates a completely random sentence
randomSentence :: Dictionary Word -> StdGen -> B.ByteString
randomSentence dic gen = ask (unwords' $ randomKey dic gen) dic gen

-- | Generate a sentence forward and backward from the given key.
ask :: B.ByteString -> Dictionary Word -> StdGen -> B.ByteString
ask key dict g =
  if sentence == key then randomSentence dict g
                     else sentence
  where
    -- generate forward and backward, concatenate and then make string
    sentence = unwords' $ reverse (drop (length key') backward)
                 ++ key' ++ (drop (length key') forward)
    -- to generate the actual key, word split the given key then let the key
    -- generator do its magic.
    key'     = toKey nopunctuation dict revOrNot
    -- we don't want any punctuation in our key; that might steal focus
    -- from important words
    nopunctuation =
      filter (\x -> not $ B.elem (B.head x) punctuation) (words' key)
    -- we might want to give preference to the later subsequences of the key
    revOrNot = fst $ randomR (True, False) g
    -- generate forward from key
    forward  = takeUntil (flip B.elem ".!?" . B.head)
                 $ disPress key' dict g
    -- generate backward from key
    backward = takeWhile (not . flip B.elem ".!?" . B.head)
                 $ disPressBack key' dict g
    takeUntil pred words =
      let (a, b) = span (not . pred) words in a ++ take 1 b

updateTextDict :: FilePath -> Dictionary Word -> IO (Dictionary Word)
updateTextDict seed oldAssocs = do
  text <- B.readFile seed >>= return . words' . B.map toLower
  return $ updateDict text oldAssocs
