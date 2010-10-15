import System.IO
import Network
import Control.Monad
import Control.Concurrent
import Control.Concurrent.MVar
import System.Environment (getArgs)
import DissociatedPress
import Data.Maybe (fromJust)
import System.Random
import Data.ByteString.Lazy.Char8 as B (take, pack, unpack, null)
import Data.Char (isSpace)

dictFile = "dictionary.bin"
defPort = 1917
version = "0.02"
protocolVersion = 2
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
     q <- getLn
     case takeWhile (not . isSpace) q of
       -- ":save" saves the current dictionary using the default dict name
       (":save") -> do
         store dictFile d
         converse d

       -- ":load" loads the default dict file
       (":load") -> do
         md' <- load dictFile
         case md' of
           Just d' -> do
             swapMVar dv d'
             converse d'
           _ ->
             converse d
       
       -- ": a bunch of text" adds "a bunch of text" to the dictionary
       (":") -> do
         let d' = id $! insertText (pack $ drop 2 q) d
         swapMVar dv d'
         converse d'

       -- any other text is interpreted as a question
       _ -> do
          text <- return . ask (pack q) d =<< newStdGen
          if B.null text
            then putLn . unpack . B.take 200 . randomSentence d =<< newStdGen
            else putLn $ unpack $ B.take 200 text
          converse d
