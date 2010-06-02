import System.IO
import Network
import Control.Monad
import Control.Concurrent
import Control.Concurrent.MVar
import System.Environment (getArgs)
import DissociatedPress
import Data.Maybe (fromJust)
import System.Random
import Data.ByteString.Lazy.Char8 (pack, unpack)

defPort = 1917
version = "0.01"
protocolVersion = 1
initString =  "server: dpress " ++ version
           ++ "\n" ++ "protocol: " ++ show protocolVersion
keyLengths d =  "maxKeyLen: " ++ show (maxKeyLen d) ++ "\n"
             ++ "preferKeyLen: " ++ show (preferKeyLen d) ++ "\n"
readyString = "> "

main = withSocketsDo $ do
  -- read dicts
  dicts <- getArgs
  hPutStrLn stderr $ "Loading dictionaries: " ++ unwords (map show dicts)
  dict <- case dicts of
    []     -> error "You must specify at least one (binary) dictionary!"
    (d:ds) -> load d >>= return . fromJust >>= \d' ->
      foldM (\a x -> load x >>= return . flip merge a . fromJust) d' ds
  dv <- newMVar dict
  hPutStrLn stderr $ "Forcing evaluation of dictionary..."
  newStdGen >>= hPutStrLn stderr . unpack . ask (pack "") dict

  -- setup network
  let myPort = defPort
  hPutStrLn stderr $ "Now listening on port " ++ show myPort
  listener <- listenOn $ PortNumber myPort

  -- accept connections
  forever $ do
    (handle, host, port) <- accept listener
    forkIO $ handleClient handle dv
 where forever x = x >> (forever x)

handleClient h dv = flip catch (\e -> return ()) $ do
  d <- readMVar dv
  putLn initString
  putLn $ keyLengths d
  converse d
 where
   putLn s = hPutStrLn h s >> hFlush h
   put   s = hPutStr h s >> hFlush h
   getLn = hGetLine h
   converse d = do
     put readyString
     -- use pack/unpack because sending chars >256 with Haskell's network
     -- library screws them up big time
     q <- getLn >>= return . pack
     newStdGen >>= putLn . unpack . ask q d
     let d' = insertText q d
     swapMVar dv d'
     converse d'
