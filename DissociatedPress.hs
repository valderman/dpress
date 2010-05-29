{-# LANGUAGE OverloadedStrings #-}
module DissociatedPress (
    Dictionary, Word,
    maxKeyLen, preferKeyLen, setPreferredKeyLength,
    newDict, defDict, updateDict, toKey, optKey,
    disPress, disPressBack, -- randomPress,
    randomSentence, ask, insertText,
    store, load
  ) where
import DissociatedPress.Core
import DissociatedPress.Storage
import DissociatedPress.Text.ByteString
