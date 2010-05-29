-- | String wrappers for the usual ByteString functions
module DissociatedPress.Text.String (
    randomSentence, ask, DTB.updateDictFromFile, words', unwords', insertText
  ) where
import System.Random
import DissociatedPress.Core
import qualified DissociatedPress.Text.ByteString as DTB
import qualified Data.ByteString.Lazy.UTF8 as BU
import Data.Char (toLower)

words' :: String -> [String]
words' = map BU.toString . DTB.words' . BU.fromString

unwords' :: [String] -> String
unwords' = BU.toString . DTB.unwords' . map BU.fromString

ask :: String -> Dictionary DTB.Word -> StdGen -> String
ask key dict gen = BU.toString $ DTB.ask (BU.fromString key) dict gen

randomSentence :: Dictionary DTB.Word -> StdGen -> String
randomSentence dict gen = BU.toString $ DTB.randomSentence dict gen

-- | Insert text into dictionary; all text is lowercased before insertion.
insertText :: String -> Dictionary DTB.Word -> Dictionary DTB.Word
insertText str = updateDict (DTB.words' $ BU.fromString
                                    $ map toLower
                                    $ str)
