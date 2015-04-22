{-# LANGUAGE MagicHash #-}
module DissociatedPress.Storage (
    load, store
  ) where
import DissociatedPress.Core
import DissociatedPress.NGram
import Data.Binary
import Codec.Compression.Zlib
import Data.ByteString.Lazy as B
import Control.Exception (catch, SomeException)

instance (Ord a, Binary a) => Binary (NGram a) where
  put (NGram w c) = put w >> put c
  get   = get >>= \w -> get >>= \c -> return $! NGram w c

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
  return $! Dictionary {
      maxKeyLen    = maxLen,
      preferKeyLen = preferLen,
      twoWay       = tw,
      dict         = d1,
      dict2        = d2
    }

store :: (Ord a, Binary a) => FilePath -> Dictionary a -> IO ()
store fp d =
  B.writeFile fp $ compress $ encode d

load :: (Ord a, Binary a) => FilePath -> IO (Maybe (Dictionary a))
load fp =
  catch (B.readFile fp >>= return . Just . decode . decompress)
        (\e -> (e :: SomeException) `seq` return Nothing)
