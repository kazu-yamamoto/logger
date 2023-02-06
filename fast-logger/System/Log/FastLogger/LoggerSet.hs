{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module System.Log.FastLogger.LoggerSet (
  -- * Creating a logger set
    LoggerSet
  , newFileLoggerSet
  , newFileLoggerSetN
  , newStdoutLoggerSet
  , newStdoutLoggerSetN
  , newStderrLoggerSet
  , newStderrLoggerSetN
  , newLoggerSet
  , newFDLoggerSet
  -- * Renewing and removing a logger set
  , renewLoggerSet
  , rmLoggerSet
  -- * Writing a log message
  , pushLogStr
  , pushLogStrLn
  -- * Flushing buffered log messages
  , flushLogStr
  -- * Misc
  , replaceLoggerSet
  ) where

import Control.Concurrent (MVar, getNumCapabilities, myThreadId, threadCapability, takeMVar, newMVar)
import Control.Debounce (mkDebounce, defaultDebounceSettings, debounceAction)
import Data.Array (Array, listArray, (!), bounds)

import System.Log.FastLogger.FileIO
import System.Log.FastLogger.IO
import System.Log.FastLogger.Imports
import System.Log.FastLogger.LogStr
import System.Log.FastLogger.Logger

----------------------------------------------------------------

-- | A set of loggers.
--   The number of loggers is the capabilities of GHC RTS.
--   You can specify it with \"+RTS -N\<x\>\".
--   A buffer is prepared for each capability.
data LoggerSet = LoggerSet {
    lgrsetFilePath :: Maybe FilePath
  , lgrsetFDRef    :: IORef FD
  , lgrsetBufSize  :: BufSize
  , lgrsetMVar     :: MVar Buffer
  , lgrsetArray    :: Array Int Logger
  , lgrsetFlush    :: IO ()
  }

-- | Creating a new 'LoggerSet' using a file.
--
-- Uses `numCapabilties` many buffers, which will result in log
-- output that is not ordered by time (see `newFileLoggerSetN`).
newFileLoggerSet :: BufSize -> FilePath -> IO LoggerSet
newFileLoggerSet size file = openFileFD file >>= newFDLoggerSet size Nothing (Just file)

-- | Creating a new 'LoggerSet' using a file, using only the given number of capabilites.
--
-- Giving @mn = Just 1@ scales less well on multi-core machines,
-- but provides time-ordered output.
newFileLoggerSetN :: BufSize -> Maybe Int -> FilePath -> IO LoggerSet
newFileLoggerSetN size mn file = openFileFD file >>= newFDLoggerSet size mn (Just file)

-- | Creating a new 'LoggerSet' using stdout.
newStdoutLoggerSet :: BufSize -> IO LoggerSet
newStdoutLoggerSet size = getStdoutFD >>= newFDLoggerSet size Nothing Nothing

-- | Creating a new 'LoggerSet' using stdout, with the given number of buffers
-- (see `newFileLoggerSetN`).
newStdoutLoggerSetN :: BufSize -> Maybe Int -> IO LoggerSet
newStdoutLoggerSetN size mn = getStdoutFD >>= newFDLoggerSet size mn Nothing

-- | Creating a new 'LoggerSet' using stderr.
newStderrLoggerSet :: BufSize -> IO LoggerSet
newStderrLoggerSet size = getStderrFD >>= newFDLoggerSet size Nothing Nothing

-- | Creating a new 'LoggerSet' using stderr, with the given number of buffers
-- (see `newFileLoggerSetN`).
newStderrLoggerSetN :: BufSize -> Maybe Int -> IO LoggerSet
newStderrLoggerSetN size mn = getStderrFD >>= newFDLoggerSet size mn Nothing

{-# DEPRECATED newLoggerSet "Use newFileLoggerSet etc instead" #-}
-- | Creating a new 'LoggerSet'.
--   If 'Nothing' is specified to the second argument,
--   stdout is used.
--   Please note that the minimum 'BufSize' is 1.
newLoggerSet :: BufSize -> Maybe Int -> Maybe FilePath -> IO LoggerSet
newLoggerSet size mn = maybe (newStdoutLoggerSet size) (newFileLoggerSetN size mn)

-- | Creating a new 'LoggerSet' using a FD.
newFDLoggerSet :: BufSize -> Maybe Int -> Maybe FilePath -> FD -> IO LoggerSet
newFDLoggerSet size mn mfile fd = do
    n <- case mn of
      Just n' -> return n'
      Nothing -> getNumCapabilities
    loggers <- replicateM n newLogger
    let arr = listArray (0,n-1) loggers
    fref <- newIORef fd
    let bufsiz = max 1 size
    mbuf <- getBuffer bufsiz >>= newMVar
    flush <- mkDebounce defaultDebounceSettings
        { debounceAction = flushLogStrRaw fref bufsiz mbuf arr
        }
    return $ LoggerSet {
        lgrsetFilePath = mfile
      , lgrsetFDRef    = fref
      , lgrsetBufSize  = bufsiz
      , lgrsetMVar     = mbuf
      , lgrsetArray    = arr
      , lgrsetFlush    = flush
      }

-- | Writing a log message to the corresponding buffer.
--   If the buffer becomes full, the log messages in the buffer
--   are written to its corresponding file, stdout, or stderr.
pushLogStr :: LoggerSet -> LogStr -> IO ()
pushLogStr LoggerSet{..} logmsg = do
    (i, _) <- myThreadId >>= threadCapability
    -- The number of capability could be dynamically changed.
    -- So, let's check the upper boundary of the array.
    let u = snd $ bounds lgrsetArray
        lim = u + 1
        j | i < lim   = i
          | otherwise = i `mod` lim
    let logger = lgrsetArray ! j
    pushLog lgrsetFDRef lgrsetBufSize lgrsetMVar logger logmsg
    lgrsetFlush

-- | Same as 'pushLogStr' but also appends a newline.
pushLogStrLn :: LoggerSet -> LogStr -> IO ()
pushLogStrLn loggerSet logStr = pushLogStr loggerSet (logStr <> "\n")

-- | Flushing log messages in buffers.
--   This function must be called explicitly when the program is
--   being terminated.
--
--   Note: Since version 2.1.6, this function does not need to be
--   explicitly called, as every push includes an auto-debounced flush
--   courtesy of the auto-update package. Since version 2.2.2, this
--   function can be used to force flushing outside of the debounced
--   flush calls.
flushLogStr :: LoggerSet -> IO ()
flushLogStr LoggerSet{..} = flushLogStrRaw lgrsetFDRef lgrsetBufSize lgrsetMVar lgrsetArray

flushLogStrRaw :: IORef FD -> BufSize -> MVar Buffer -> Array Int Logger -> IO ()
flushLogStrRaw fdref size mbuf arr = do
    let (l,u) = bounds arr
    mapM_ flushIt [l .. u]
  where
    flushIt i = flushLog fdref size mbuf (arr ! i)

-- | Renewing the internal file information in 'LoggerSet'.
--   This does nothing for stdout and stderr.
renewLoggerSet :: LoggerSet -> IO ()
renewLoggerSet LoggerSet{..} = case lgrsetFilePath of
  Nothing -> return ()
  Just file -> do
      newfd <- openFileFD file
      oldfd <- atomicModifyIORef' lgrsetFDRef (\fd -> (newfd, fd))
      closeFD oldfd

-- | Flushing the buffers, closing the internal file information
--   and freeing the buffers.
rmLoggerSet :: LoggerSet -> IO ()
rmLoggerSet LoggerSet{..} = do
    fd <- readIORef lgrsetFDRef
    when (isFDValid fd) $ do
        let (l,u) = bounds lgrsetArray
        let nums = [l .. u]
        mapM_ flushIt nums
        takeMVar lgrsetMVar >>= freeBuffer
        when (isJust lgrsetFilePath) $ closeFD fd
        writeIORef lgrsetFDRef invalidFD
  where
    flushIt i = flushLog lgrsetFDRef lgrsetBufSize lgrsetMVar (lgrsetArray ! i)

-- | Replacing the file path in 'LoggerSet' and returning a new
--   'LoggerSet' and the old file path.
replaceLoggerSet :: LoggerSet -> FilePath -> (LoggerSet, Maybe FilePath)
replaceLoggerSet lgrset@LoggerSet{..} new_file_path =
    (lgrset { lgrsetFilePath = Just new_file_path }, lgrsetFilePath)
