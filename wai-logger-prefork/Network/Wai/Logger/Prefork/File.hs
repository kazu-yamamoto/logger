module Network.Wai.Logger.Prefork.File where

import Control.Applicative
import Control.Concurrent
import Control.Exception (handle, SomeException, catch)
import Control.Monad
import Data.IORef
import Network.Wai.Logger
import Network.Wai.Logger.Prefork.Types
import Prelude hiding (catch)
import System.IO
import System.Log.FastLogger
import System.Posix

----------------------------------------------------------------

newtype LoggerRef = LoggerRef (IORef Logger)

getLogger :: LoggerRef -> IO Logger
getLogger (LoggerRef ref) = readIORef ref

setLogger :: LoggerRef -> Logger -> IO ()
setLogger (LoggerRef ref) logger = writeIORef ref logger

----------------------------------------------------------------

fileLoggerInit :: IPAddrSource -> FileLogSpec -> IO ApacheLogger
fileLoggerInit ipsrc spec = do
    hdl <- open spec
    logger <- mkLogger False hdl
    logref <- LoggerRef <$> newIORef logger
    _ <- forkIO $ fileFlusher logref
    _ <- installHandler sigUSR1 (Catch $ reopen spec logref) Nothing
    return $ fileLogger ipsrc logref

{-
 For BlockBuffering, hPut flushes the buffer before writing
 the target string. In other words, hPut does not split
 the target string. So, to implment multiple line buffering,
 just use BlockBuffering.
-}
open :: FileLogSpec -> IO Handle
open spec = do
    hdl <- openFile (log_file spec) AppendMode
    initHandle hdl
    return hdl

reopen :: FileLogSpec -> LoggerRef -> IO ()
reopen spec logref = do
    oldlogger <- getLogger logref
    loggerFlush oldlogger
    let oldhdl = loggerHandle oldlogger
    hClose oldhdl
    newhdl <- open spec
    let newlogger = oldlogger { loggerHandle = newhdl }
    setLogger logref newlogger

----------------------------------------------------------------

fileLogger :: IPAddrSource -> LoggerRef -> ApacheLogger
fileLogger ipsrc logref req status msiz = do
    logger <- getLogger logref
    date <- loggerDate logger
    loggerPutStr logger $ apacheFormat ipsrc date req status msiz

fileFlusher :: LoggerRef -> IO ()
fileFlusher logref = forever $ do
    threadDelay 10000000
    getLogger logref >>= loggerFlush

----------------------------------------------------------------

fileLoggerController :: FileLogSpec -> LogController
fileLoggerController spec pids = forever $ do
    isOver <- over
    when isOver $ do
        rotate spec
        mapM_ sendSignal pids
    threadDelay 10000000
  where
    file = log_file spec
    over = handle handler $ do
        siz <- fromIntegral . fileSize <$> getFileStatus file
        if siz > log_file_size spec then
            return True
          else
            return False
    sendSignal pid = signalProcess sigUSR1 pid `catch` ignore
    handler :: SomeException -> IO Bool
    handler _ = return False
    ignore :: SomeException -> IO ()
    ignore _ = return ()
