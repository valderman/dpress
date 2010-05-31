import DissociatedPress
import System.Environment (getArgs)
import Control.Monad (foldM)
import Data.Maybe (fromJust)

main = do
  dicts <- getArgs
  d <- case dicts of
    []     -> error "Specify the dictionaries to merge on the command line."
    (d:ds) -> load d >>= return . fromJust >>= \d' ->
      foldM (\a x -> load x >>= return . flip merge a . fromJust) d' ds
  store "out.dict" (d :: Dictionary Word)
