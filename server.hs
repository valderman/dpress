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
initString =  "dpress " ++ version
           ++ "\n" ++ "protocol: " ++ show protocolVersion
keyLengths d =  "maxKeyLen: " ++ show (maxKeyLen d) ++ "\n"
             ++ "preferKeyLen: " ++ show (preferKeyLen d)
readyString = "> "

main = withSocketsDo $ do
  -- read dicts
  dicts <- getArgs
  hPutStrLn stderr $ "Loading dictionaries: " ++ unwords (map show dicts)
  dict <- case dicts of
    [] -> error "You must specify at least one (binary) dictionary!"
    _  -> foldM (\a x -> load x >>= return . flip merge a . fromJust)
                (defDict :: Dictionary Word) dicts
  dv <- newMVar dict

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
     q <- getLn >>= return . pack
     newStdGen >>= putLn . unpack . ask q d
     converse d
