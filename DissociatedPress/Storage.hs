module DissociatedPress.Storage (
    load, store
  ) where
import DissociatedPress.Core
import Data.Binary
import Data.ByteString.Lazy as B

instance (Ord a, Binary a) => Binary (Dictionary a) where
  put = putD
  get = getD

putD :: (Ord a, Binary a) => Dictionary a -> Put
putD d = do
  put $ maxKeyLen d
  put $ preferKeyLen d
  put $ twoWay d
  put $ dict d
  put $ dict2 d

getD :: (Ord a, Binary a) => Get (Dictionary a)
getD = do
  maxLen <- get
  preferLen <- get
  tw <- get
  d1 <- get
  d2 <- get
  return $ Dictionary {
      maxKeyLen = maxLen,
      preferKeyLen = preferLen,
      twoWay = tw,
      dict = d1,
      dict2 = d2
    }

store :: (Ord a, Binary a) => FilePath -> Dictionary a -> IO ()
store fp d =
  B.writeFile fp $ encode d

load :: (Ord a, Binary a) => FilePath -> IO (Dictionary a)
load fp =
  B.readFile fp >>= return . decode
