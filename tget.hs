import Network
import qualified Data.ByteString.Lazy.Char8 as B
import System.Environment (getArgs)
import System.IO

usageMsg =  "Usage: tget host port [question]\n"
         ++ "If question is omitted, it is read from stdin."

main = withSocketsDo $ do
  (host, port, q) <- getArgs >>= \as -> flip catch (\_ -> error usageMsg) $ do
    stdin <- B.getContents >>= return . B.unpack
    if length as < 2
       then error usageMsg
       else return ()
    if length as < 3
       then return (as !! 0, read $ as !! 1, stdin)
       else return (as !! 0, read $ as !! 1, unwords $ drop 2 as)
  h <- connectTo host (PortNumber $ fromIntegral port)
  hSetBuffering h LineBuffering
  opts <- readOpts h
  checkOpts opts
  hPutStrLn h q >> hFlush h
  res <- B.hGetContents h >>= return . B.drop 2 . head . B.lines
  B.putStrLn res

readOpts :: Handle -> IO [String]
readOpts h = do
  opt <- hGetLine h
  if null opt
     then return []
     else readOpts h >>= return . (opt:)

checkOpts :: [String] -> IO ()
checkOpts = mapM_ checkVer . map words
  where checkVer ("protocol:":["1"]) = return ()
        checkVer ("protocol:":_)     = error $  "Server speaks a newer version "
                                             ++ "of the protocol!"
        checkVer _                   = return ()
