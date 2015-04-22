{-# LANGUAGE OverloadedStrings #-}
module DissociatedPress (
    Dictionary, Word,
    maxKeyLen, preferKeyLen, setPreferredKeyLength,
    newDict, defDict, updateDict, toKey, optKey, merge,
    disPress, disPressBack, randomPress, randomKey,
    randomSentence, ask, insertText,
    store, load,
    words', unwords'
  ) where
import DissociatedPress.Core
import DissociatedPress.Storage
import DissociatedPress.Text
