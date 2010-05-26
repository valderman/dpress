module DissociatedPress (
    Dictionary, Word,
    newDict, defDict, updateDict, setPreferredKeyLength,
    disPress, disPressBack, randomPress,
    randomSentence, ask, updateTextDict,
    store, load
  ) where
import DissociatedPress.Core
import DissociatedPress.Storage
import DissociatedPress.Text
