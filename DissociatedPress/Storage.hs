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
import Control.Applicative

instance Binary NGram where
  put (NGram w c) = put w >> put c
  get   = get >>= \w -> get >>= \c -> return $! NGram w c

instance Binary a => Binary (Dictionary a) where
  put d = do
    put $ maxKeyLen d
    put $ preferKeyLen d
    put $ twoWay d
    put $ dict d
    put $ dict2 d
    put $ wordMap d
  get = Dictionary <$> get <*> get <*> get <*> get <*> get <*> get

store :: (Ord a, Binary a) => FilePath -> Dictionary a -> IO ()
store fp d =
  B.writeFile fp $ compress $ encode d

load :: (Ord a, Binary a) => FilePath -> IO (Maybe (Dictionary a))
load fp =
  catch (B.readFile fp >>= return . Just . decode . decompress)
        (\e -> (e :: SomeException) `seq` return Nothing)
