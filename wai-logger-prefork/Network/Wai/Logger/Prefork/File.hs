{-# LANGUAGE DoAndIfThenElse #-}

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

logBufSize :: Int
logBufSize = 4096

----------------------------------------------------------------

newtype HandleRef = HandleRef (IORef Handle)

getHandle :: HandleRef -> IO Handle
getHandle (HandleRef ref) = readIORef ref

----------------------------------------------------------------

fileLoggerInit :: IPAddrSource -> FileLogSpec -> IO ApacheLogger
fileLoggerInit ipsrc spec = do
    hdl <- open spec
    hdlref <- HandleRef <$> newIORef hdl
    forkIO $ fileFlusher hdlref
    dateref <- dateInit
    installHandler sigUSR1 (Catch $ reopen spec hdlref) Nothing
    return $ fileLogger ipsrc dateref hdlref

{-
 For BlockBuffering, hPut flushes the buffer before writing
 the target string. In other words, hPut does not split
 the target string. So, to implment multiple line buffering,
 just use BlockBuffering.
-}
open :: FileLogSpec -> IO Handle
open spec = do
    hdl <- openFile file AppendMode
    hSetBuffering hdl (BlockBuffering (Just logBufSize))
    return hdl
  where
    file = log_file spec

reopen :: FileLogSpec -> HandleRef -> IO ()
reopen spec (HandleRef ref) = do
    oldhdl <- readIORef ref
    open spec >>= writeIORef ref
    hClose oldhdl

----------------------------------------------------------------

fileLogger :: IPAddrSource -> DateRef -> HandleRef -> ApacheLogger
fileLogger ipsrc dateref hdlref req status msiz = do
    date <- getDate dateref
    hdl <- getHandle hdlref
    hPutLogStr hdl $ apacheFormat ipsrc date req status msiz

fileFlusher :: HandleRef -> IO ()
fileFlusher hdlref = forever $ do
    threadDelay 10000000
    getHandle hdlref >>= hFlush

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
