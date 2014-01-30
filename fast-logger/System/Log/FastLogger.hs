-- | This module provides a fast logging system which
--   scales on multicore environments (i.e. +RTS -N\<x\>).
{-# LANGUAGE BangPatterns, CPP #-}

module System.Log.FastLogger (
  -- * Creating a logger set
    LoggerSet
  , newFileLoggerSet
  , newStdoutLoggerSet
  , newStderrLoggerSet
  , newLoggerSet
  -- * Buffer size
  , BufSize
  , defaultBufSize
  -- * Renewing and removing a logger set
  , renewLoggerSet
  , rmLoggerSet
  -- * Log messages
  , LogStr
  , ToLogStr(..)
  , fromLogStr
  , logStrLength
  -- * Writing a log message
  , pushLogStr
  -- * Flushing buffered log messages
  , flushLogStr
  -- * File rotation
  , module System.Log.FastLogger.File
  ) where

import Control.Applicative ((<$>))
import Control.Concurrent (getNumCapabilities, myThreadId, threadCapability, takeMVar)
import Control.Monad (when, replicateM)
import Data.Array (Array, listArray, (!), bounds)
import Data.Maybe (isJust)
import GHC.IO.Device (close)
import GHC.IO.FD (FD(..), openFile, stderr, stdout)
import GHC.IO.IOMode (IOMode(..))
import System.Log.FastLogger.File
import System.Log.FastLogger.IO
import System.Log.FastLogger.IORef
import System.Log.FastLogger.LogStr
import System.Log.FastLogger.Logger

----------------------------------------------------------------

-- | Opening a log file.
logOpen :: FilePath -> IO FD
logOpen file = fst <$> openFile file AppendMode False

----------------------------------------------------------------

-- | A set of loggers.
--   The number of loggers is the capabilities of GHC RTS.
--   You can specify it with \"+RTS -N\<x\>\".
--   A buffer is prepared for each capability.
data LoggerSet = LoggerSet (Maybe FilePath) (IORef FD) (Array Int Logger)

-- | Creating a new 'LoggerSet' using a file.
newFileLoggerSet :: BufSize -> FilePath -> IO LoggerSet
newFileLoggerSet size file = logOpen file >>= newFDLoggerSet size (Just file)

-- | Creating a new 'LoggerSet' using stdout.
newStdoutLoggerSet :: BufSize -> IO LoggerSet
newStdoutLoggerSet size = newFDLoggerSet size Nothing stdout

-- | Creating a new 'LoggerSet' using stderr.
newStderrLoggerSet :: BufSize -> IO LoggerSet
newStderrLoggerSet size = newFDLoggerSet size Nothing stderr

{-# DEPRECATED newLoggerSet "Use newFileLoggerSet etc instead" #-}
-- | Creating a new 'LoggerSet'.
--   If 'Nothing' is specified to the second argument,
--   stdout is used.
--   Please note that the minimum 'BufSize' is 1.
newLoggerSet :: BufSize -> Maybe FilePath -> IO LoggerSet
newLoggerSet size = maybe (newStdoutLoggerSet size) (newFileLoggerSet size)

-- | Creating a new 'LoggerSet' using a FD.
newFDLoggerSet :: BufSize -> Maybe FilePath -> FD -> IO LoggerSet
newFDLoggerSet size mfile fd = do
    n <- getNumCapabilities
    loggers <- replicateM n $ newLogger (max 1 size)
    let arr = listArray (0,n-1) loggers
    fref <- newIORef fd
    return $ LoggerSet mfile fref arr

-- | Writing a log message to the corresponding buffer.
--   If the buffer becomes full, the log messages in the buffer
--   are written to its corresponding file, stdout, or stderr.
pushLogStr :: LoggerSet -> LogStr -> IO ()
pushLogStr (LoggerSet _ fref arr) logmsg = do
    (i, _) <- myThreadId >>= threadCapability
    -- The number of capability could be dynamically changed.
    -- So, let's check the upper boundary of the array.
    let u = snd $ bounds arr
        lim = u + 1
        j | i < lim   = i
          | otherwise = i `mod` lim
    let logger = arr ! j
    fd <- readIORef fref
    pushLog fd logger logmsg

-- | Flushing log messages in buffers.
--   This function must be called explicitly when the program is
--   being terminated.
flushLogStr :: LoggerSet -> IO ()
flushLogStr (LoggerSet _ fref arr) = do
    n <- getNumCapabilities
    fd <- readIORef fref
    mapM_ (flushIt fd) [0..n-1]
  where
    flushIt fd i = flushLog fd (arr ! i)

-- | Renewing the internal file information in 'LoggerSet'.
--   This does nothing for stdout and stderr.
renewLoggerSet :: LoggerSet -> IO ()
renewLoggerSet (LoggerSet Nothing     _    _) = return ()
renewLoggerSet (LoggerSet (Just file) fref _) = do
    newfd <- logOpen file
    oldfd <- atomicModifyIORef' fref (\fd -> (newfd, fd))
    close oldfd

-- | Flushing the buffers, closing the internal file information
--   and freeing the buffers.
rmLoggerSet :: LoggerSet -> IO ()
rmLoggerSet (LoggerSet mfile fref arr) = do
    n <- getNumCapabilities
    fd <- readIORef fref
    let nums = [0..n-1]
    mapM_ (flushIt fd) nums
    mapM_ freeIt nums
    when (isJust mfile) $ close fd
  where
    flushIt fd i = flushLog fd (arr ! i)
    freeIt i = do
        let (Logger mbuf _ _) = arr ! i
        takeMVar mbuf >>= freeBuffer
