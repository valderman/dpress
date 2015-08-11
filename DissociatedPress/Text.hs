{-# LANGUAGE CPP, OverloadedStrings #-}
-- | Less general functions for working with textual data.
module DissociatedPress.Text (
    Word,
    randomSentence, ask, updateDictFromFile, words', unwords', insertText
  ) where
import qualified Data.Text as T
import Data.Char (toLower)
import System.Random
import DissociatedPress.Core
import Data.List (foldl')
#if __GLASGOW_HASKELL__ > 708
import Prelude hiding (Word)
#endif

type Word = T.Text

-- | Definitions for words' and unwords'
whitespace, punctuation, ignore, nonword :: T.Text
whitespace  = " \n\t"
punctuation = ",.!?:;*/&="
ignore      = "\r()\""
nonword     = T.concat [whitespace, punctuation]

newtype WordsState = WS Int deriving Eq

stSpace, stPunct, stWord :: WordsState
stSpace = WS 0
stPunct = WS 1
stWord  = WS 2

isElem :: Char -> T.Text -> Bool
isElem c s = T.singleton c `T.isInfixOf` s

-- | "Better" words; recognizes punctuation as separate words.
words' :: T.Text -> [Word]
words' = go stSpace . T.filter (not . (`isElem` ignore))
  where
    go st s
      | T.null s = []
      | st == stSpace =
        let (_, s') = T.span (`isElem` whitespace) s
            st'     = if isElem (T.head s') punctuation
                         then stPunct
                         else stWord
        in go st' s'
      | st == stWord =
        let (w, s') = T.span (not . (`isElem` nonword)) s
            st'     = if isElem (T.head s') punctuation
                         then stPunct
                         else stSpace
        in w:go st' s'
      | st == stPunct =
        let (w, s') = T.span (`isElem` punctuation) s
            st'     = if isElem (T.head s') whitespace
                         then stSpace
                         else stWord
        in w:go st' s'

-- | "Better" unwords; doesn't put spaces before punctuation.
unwords' :: [Word] -> T.Text
unwords' = T.concat . unwords'' where
  unwords'' (a:b:ws)
    | isElem (T.head b) punctuation = a:unwords'' (b:ws)
    | otherwise                     = a:unwords'' ((' ' `T.cons` b):ws)
  unwords'' (w:[])                  = [w]
  unwords'' []                      = []

-- | Generates a completely random sentence
randomSentence :: Dictionary Word -> StdGen -> T.Text
randomSentence dic gen = ask (unwords' $ randomKey dic gen) dic gen

-- | Generate a sentence forward and backward from the given key.
--   We also filter out any punctuation from the keys, as punctuation is too
--   common to be in any good key.
ask :: T.Text -> Dictionary Word -> StdGen -> T.Text
ask key dic g =
  if T.null sentence
     then T.empty
     else sentence
  where
    takeUntil pred words =
      let (a, b) = span (not . pred) words in a ++ take 1 b
    
    -- lowercase key then split into words
    lowercaseKey  = words' $ T.map toLower key
    
    -- we don't want any punctuation in our key; that might steal focus
    -- from important words
    nopunctuation =
      filter (\x -> not $ isElem (T.head x) punctuation) lowercaseKey
    
    -- to generate the actual key, word split the given key then let the key
    -- generator do its magic.
    key' = toKey nopunctuation dic g
    
    -- generate forward from key
    forward  = takeUntil (flip isElem ".!?" . T.head) $ disPress key' dic g
    
    -- generate backward from key
    backward = takeWhile (not . flip isElem ".!?" . T.head)
                   $ disPressBack key' dic g
    
    -- generate forward and backward, concatenate and then make string
    sentence = unwords' $  reverse (drop (length key') backward)
                        ++ key' ++ (drop (length key') forward)

-- | Updates a dictionary from a file; all words are lowercased.
updateDictFromFile :: FilePath -> Dictionary Word -> IO (Dictionary Word)
updateDictFromFile seed oldAssocs = do
  str <- readFile seed
  return $ insertText (T.pack str) oldAssocs

-- | Insert text into dictionary; all text is lowercased before insertion.
insertText :: T.Text -> Dictionary Word -> Dictionary Word
insertText s d =
    foldl' (\dict str -> updateDict (words' $ T.map toLower str) dict) d ls
  where
    ls = map addFullStop $ filter (not . T.null) $ T.lines s
    addFullStop l
      | T.last l `elem` (".!?" :: String) = l
      | otherwise                         = T.snoc l '.'
