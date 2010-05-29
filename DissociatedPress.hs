{-# LANGUAGE OverloadedStrings #-}
module DissociatedPress (
    Dictionary, Word,
    newDict, defDict, updateDict, setPreferredKeyLength, toKey, optKey,
    disPress, disPressBack, -- randomPress,
    randomSentence, ask, insertText,
    store, load
  ) where
import Data.ByteString.Lazy.Char8
import DissociatedPress.Core
import DissociatedPress.Storage
import DissociatedPress.Text.ByteString
