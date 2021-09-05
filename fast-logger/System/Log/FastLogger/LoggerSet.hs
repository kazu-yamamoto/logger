{-# LANGUAGE OverloadedStrings #-}

module System.Log.FastLogger.LoggerSet (
  -- * Creating a logger set
    LoggerSet
  , newFileLoggerSet
  , newFileLoggerSetN
  , newStdoutLoggerSet
  , newStderrLoggerSet
  , newLoggerSet
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

import Control.Debounce (mkDebounce, defaultDebounceSettings, debounceAction)
import Control.Concurrent (getNumCapabilities, myThreadId, threadCapability, MVar, newMVar, takeMVar)
import Control.Exception (mask_)
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
data LoggerSet = LoggerSet (Maybe FilePath) (MVar FD) (IORef (Array Int Logger)) (IO ())

-- | Creating a new 'LoggerSet' using a file.
newFileLoggerSet :: BufSize -> FilePath -> IO LoggerSet
newFileLoggerSet size file = openFileFD file >>= newFDLoggerSet size Nothing (Just file)

-- | Creating a new 'LoggerSet' using a file.
newFileLoggerSetN :: BufSize -> Maybe Int -> FilePath -> IO LoggerSet
newFileLoggerSetN size mn file = openFileFD file >>= newFDLoggerSet size mn (Just file)

-- | Creating a new 'LoggerSet' using stdout.
newStdoutLoggerSet :: BufSize -> IO LoggerSet
newStdoutLoggerSet size = getStdoutFD >>= newFDLoggerSet size Nothing Nothing

-- | Creating a new 'LoggerSet' using stderr.
newStderrLoggerSet :: BufSize -> IO LoggerSet
newStderrLoggerSet size = getStderrFD >>= newFDLoggerSet size Nothing Nothing

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
    loggers <- replicateM n $ newLogger (max 1 size)
    let arr = listArray (0,n-1) loggers
    arrref <- newIORef arr
    fdmv <- newMVar fd
    flush <- mkDebounce defaultDebounceSettings
        { debounceAction = flushLogStrRaw fdmv arrref
        }
    return $ LoggerSet mfile fdmv arrref flush

-- | Writing a log message to the corresponding buffer.
--   If the buffer becomes full, the log messages in the buffer
--   are written to its corresponding file, stdout, or stderr.
pushLogStr :: LoggerSet -> LogStr -> IO ()
pushLogStr (LoggerSet _ fdref arrref flush) logmsg = do
    arr <- readIORef arrref
    (i, _) <- myThreadId >>= threadCapability
    -- The number of capability could be dynamically changed.
    -- So, let's check the upper boundary of the array.
    let u = snd $ bounds arr
        lim = u + 1
        j | i < lim   = i
          | otherwise = i `mod` lim
    let logger = arr ! j
    pushLog fdref logger logmsg
    flush

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
flushLogStr (LoggerSet _ fref arrref _) = flushLogStrRaw fref arrref

flushLogStrRaw :: MVar FD -> IORef (Array Int Logger) -> IO ()
flushLogStrRaw fdmv arrref = do
    arr <- readIORef arrref
    let (l,u) = bounds arr
    mapM_ (flushIt arr) [l .. u]
  where
    flushIt arr i = flushLog fdmv (arr ! i)

-- | Renewing the internal file information in 'LoggerSet'.
--   This does nothing for stdout and stderr.
renewLoggerSet :: LoggerSet -> IO ()
renewLoggerSet (LoggerSet Nothing     _    _ _) = return ()
renewLoggerSet (LoggerSet (Just file) fdmv _ _) = do
    newfd <- openFileFD file
    oldfd <- modifyFd fdmv (\fd -> pure (newfd, fd))
    closeFD oldfd

-- | Flushing the buffers, closing the internal file information
--   and freeing the buffers.
rmLoggerSet :: LoggerSet -> IO ()
rmLoggerSet (LoggerSet mfile fdmv arrref _) =
    -- Mask during finalization to prevent losing log messages
    mask_ $ join $ do
        -- Taking a lock of 'MVar FD' means blocking all threads to write to FD.
        -- Therefore lock time should be short.
        modifyFd fdmv $ \fd ->
            if isFDValid fd
                then do
                    -- Replace current 'Logger's with new ones to prevent that
                    -- another thread consumes messages in current 'Logger's after
                    -- unlock 'MVar FD'.
                    newArr <- newSameSizeEmptyLoggers arrref
                    closingArr <- atomicModifyIORef' arrref $ \curArr -> (newArr, curArr)
                    pure (invalidFD, finalizeAfterUnlock fd closingArr)
                else pure (fd, pure ())
  where
    newSameSizeEmptyLoggers :: IORef (Array Int Logger) -> IO (Array Int Logger)
    newSameSizeEmptyLoggers arrref' = do
        tmpArr <- readIORef arrref'
        let len = length tmpArr
            Logger bufsize _ _ = tmpArr ! 0
        loggers <- replicateM len $ newLogger (max 1 bufsize)
        pure $ listArray (0,len-1) loggers
    finalizeAfterUnlock :: FD -> Array Int Logger ->  IO ()
    finalizeAfterUnlock fd arr = do
        let (l,u) = bounds arr
        let nums = [l .. u]
        fdmv' <- newMVar fd -- NOTE: Just to pass it to `flushLog`
        mapM_ (flushIt fdmv' arr) nums
        mapM_ (freeIt arr) nums
        when (isJust mfile) $ closeFD fd
    flushIt fdmv' arr i = flushLog fdmv' (arr ! i)
    freeIt arr i = do
        let (Logger _ mbuf _) = arr ! i
        takeMVar mbuf >>= freeBuffer

-- | Replacing the file path in 'LoggerSet' and returning a new
--   'LoggerSet' and the old file path.
replaceLoggerSet :: LoggerSet -> FilePath -> (LoggerSet, Maybe FilePath)
replaceLoggerSet (LoggerSet current_path a b c) new_file_path =
    (LoggerSet (Just new_file_path) a b c, current_path)
