{-# LANGUAGE BangPatterns, CPP #-}

module System.Log.FastLogger (
  -- * Creating a logger set
    LoggerSet
  , BufSize
  , defaultBufSize
  , logOpen
  , newLoggerSet
  , renewLoggerSet
  -- * Removing a logger set
  , rmLoggerSet
  -- * Log messages
  , LogStr
  , ToLogStr(..)
  , logStrLength
  , logStrBuilder
  -- * Writing a log message
  , pushLogStr
  -- * Flushing buffered log messages
  , flushLogStr
  -- * File rotation
  , module System.Log.FastLogger.File
  ) where

import Control.Applicative ((<$>))
import Control.Concurrent (getNumCapabilities, myThreadId, threadCapability, MVar, newMVar, takeMVar, withMVar)
import Control.Monad (when, replicateM)
import Data.Array (Array, listArray, (!))
import Data.IORef
import Foreign.Marshal.Alloc (mallocBytes, free)
import Foreign.Ptr (plusPtr)
import GHC.IO.Device (close)
import GHC.IO.FD (FD(..), openFile, writeRawBufferPtr)
import GHC.IO.IOMode (IOMode(..))
import System.Log.FastLogger.File
import System.Log.FastLogger.LogStr
import System.Log.FastLogger.IO

----------------------------------------------------------------

-- | Writting 'LogStr' using a buffer in blocking mode.
--   The size of 'LogStr' must be smaller or equal to
--   the size of buffer.
writeLogStr :: FD
            -> Buffer
            -> BufSize
            -> LogStr
            -> IO ()
writeLogStr fd buf size (LogStr len builder)
  | size < len = error "writeLogStr"
  | otherwise  = toBufIOWith buf size (write fd) builder

write :: FD -> Buffer -> Int -> IO ()
write fd buf len' = loop buf (fromIntegral len')
  where
    loop bf !len = do
        written <- writeRawBufferPtr "write" fd bf 0 (fromIntegral len)
        when (written < len) $
            loop (bf `plusPtr` fromIntegral written) (len - written)

----------------------------------------------------------------

data Logger = Logger (MVar Buffer) !BufSize (IORef LogStr)

newLogger :: BufSize -> IO Logger
newLogger size = do
    buf <- getBuffer size
    mbuf <- newMVar buf
    lref <- newIORef mempty
    return $ Logger mbuf size lref

pushLog :: FD -> Logger -> LogStr -> IO ()
pushLog fd logger@(Logger mbuf size ref) nlogmsg@(LogStr nlen nbuilder)
  | nlen > size = do
      flushLog fd logger
      withMVar mbuf $ \buf -> toBufIOWith buf nlen (write fd) nbuilder
  | otherwise = do
    needFlush <- atomicModifyIORef' ref checkBuf
    when needFlush $ do
        flushLog fd logger
        pushLog fd logger nlogmsg
  where
    checkBuf ologmsg@(LogStr olen _)
      | size < olen + nlen = (ologmsg, True)
      | otherwise          = (ologmsg <> nlogmsg, False)

flushLog :: FD -> Logger -> IO ()
flushLog fd (Logger mbuf size lref) = do
    logmsg <- atomicModifyIORef' lref (\old -> (mempty, old))
    -- If a special buffer is prepared for flusher, this MVar could
    -- be removed. But such a code does not contribute logging speed
    -- according to experiment. And even with the special buffer,
    -- there is no grantee that this function is exclusively called
    -- for a buffer. So, we use MVar here.
    -- This is safe and speed penalty can be ignored.
    withMVar mbuf $ \buf -> writeLogStr fd buf size logmsg

----------------------------------------------------------------

-- | Opening a log file.
logOpen :: FilePath -> IO FD
logOpen file = fst <$> openFile file AppendMode False

getBuffer :: BufSize -> IO Buffer
getBuffer = mallocBytes

----------------------------------------------------------------

-- | A set of loggers.
--   The number of loggers is the capabilities of GHC RTS.
--   You can specify it with \"+RTS -N\<x\>\".
--   A buffer is prepared for each capability.
data LoggerSet = LoggerSet (IORef FD) (Array Int Logger)

-- | Creating a new 'LoggerSet'.
newLoggerSet :: BufSize -> FD -> IO LoggerSet
newLoggerSet size fd = do
    n <- getNumCapabilities
    loggers <- replicateM n $ newLogger size
    let arr = listArray (0,n-1) loggers
    fref <- newIORef fd
    return $ LoggerSet fref arr

-- | Writing a log message to the corresponding buffer.
pushLogStr :: LoggerSet -> LogStr -> IO ()
pushLogStr (LoggerSet fref arr) logmsg = do
    (i, _) <- myThreadId >>= threadCapability
    let logger = arr ! i
    fd <- readIORef fref
    pushLog fd logger logmsg

-- | Flushing log messages in buffers.
flushLogStr :: LoggerSet -> IO ()
flushLogStr (LoggerSet fref arr) = do
    n <- getNumCapabilities
    fd <- readIORef fref
    mapM_ (flushIt fd) [0..n-1]
  where
    flushIt fd i = flushLog fd (arr ! i)

-- | Renewing 'FD' in 'LoggerSet'. Old 'FD' is closed.
renewLoggerSet :: LoggerSet -> FD -> IO ()
renewLoggerSet (LoggerSet fref _) newfd = do
    oldfd <- atomicModifyIORef' fref (\fd -> (newfd, fd))
    close oldfd

-- | Flushing the buffers, closing 'FD' and freeing the buffers.
rmLoggerSet :: LoggerSet -> IO ()
rmLoggerSet (LoggerSet fref arr) = do
    n <- getNumCapabilities
    fd <- readIORef fref
    let nums = [0..n-1]
    mapM_ (flushIt fd) nums
    mapM_ freeIt nums
    when (fdFD fd /= 1) $ close fd
  where
    flushIt fd i = flushLog fd (arr ! i)
    freeIt i = do
        let (Logger mbuf _ _) = arr ! i
        takeMVar mbuf >>= free

#if !MIN_VERSION_base(4,6,0)
-- | Strict version of 'atomicModifyIORef'.  This forces both the value stored
-- in the 'IORef' as well as the value returned.
atomicModifyIORef' :: IORef a -> (a -> (a,b)) -> IO b
atomicModifyIORef' ref f = do
    c <- atomicModifyIORef ref
            (\x -> let (a, b) = f x    -- Lazy application of "f"
                    in (a, a `seq` b)) -- Lazy application of "seq"
    -- The following forces "a `seq` b", so it also forces "f x".
    c `seq` return c
#endif
