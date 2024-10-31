{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module System.Log.FastLogger.LoggerSet (
    -- * Creating a logger set
    LoggerSet,
    newFileLoggerSet,
    newFileLoggerSetN,
    newStdoutLoggerSet,
    newStdoutLoggerSetN,
    newStderrLoggerSet,
    newStderrLoggerSetN,
    newLoggerSet,
    newFDLoggerSet,

    -- * Renewing and removing a logger set
    renewLoggerSet,
    rmLoggerSet,

    -- * Writing a log message
    pushLogStr,
    pushLogStrLn,

    -- * Flushing buffered log messages
    flushLogStr,

    -- * Misc
    replaceLoggerSet,
) where

import Control.Concurrent (getNumCapabilities)
import Control.Debounce (debounceAction, defaultDebounceSettings, mkDebounce, debounceThreadName)

import System.Log.FastLogger.FileIO
import System.Log.FastLogger.IO
import System.Log.FastLogger.Imports
import System.Log.FastLogger.LogStr
import System.Log.FastLogger.MultiLogger (MultiLogger)
import qualified System.Log.FastLogger.MultiLogger as M
import System.Log.FastLogger.SingleLogger (SingleLogger)
import qualified System.Log.FastLogger.SingleLogger as S
import System.Log.FastLogger.Write

----------------------------------------------------------------

data Logger = SL SingleLogger | ML MultiLogger

----------------------------------------------------------------

-- | A set of loggers.
--   The number of loggers is the capabilities of GHC RTS.
--   You can specify it with \"+RTS -N\<x\>\".
--   A buffer is prepared for each capability.
data LoggerSet = LoggerSet
    { lgrsetFilePath :: Maybe FilePath
    , lgrsetFdRef :: IORef FD
    , lgrsetLogger :: Logger
    , lgrsetDebounce :: IO ()
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
    fdref <- newIORef fd
    let bufsiz = max 1 size
    logger <-
        if n == 1 && mn == Just 1
            then
                SL <$> S.newSingleLogger bufsiz fdref
            else do
                ML <$> M.newMultiLogger n bufsiz fdref
    flush <-
        mkDebounce
            defaultDebounceSettings
                { debounceAction = flushLogStrRaw logger
                , debounceThreadName = "Loggerset of FastLogger (Debounce)"
                }
    return $
        LoggerSet
            { lgrsetFilePath = mfile
            , lgrsetFdRef = fdref
            , lgrsetLogger = logger
            , lgrsetDebounce = flush
            }

-- | Writing a log message to the corresponding buffer.
--   If the buffer becomes full, the log messages in the buffer
--   are written to its corresponding file, stdout, or stderr.
pushLogStr :: LoggerSet -> LogStr -> IO ()
pushLogStr LoggerSet{..} logmsg = case lgrsetLogger of
    SL sl -> do
        pushLog sl logmsg
        lgrsetDebounce
    ML ml -> do
        pushLog ml logmsg
        lgrsetDebounce

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
flushLogStr LoggerSet{..} = flushLogStrRaw lgrsetLogger

flushLogStrRaw :: Logger -> IO ()
flushLogStrRaw (SL sl) = flushAllLog sl
flushLogStrRaw (ML ml) = flushAllLog ml

-- | Renewing the internal file information in 'LoggerSet'.
--   This does nothing for stdout and stderr.
renewLoggerSet :: LoggerSet -> IO ()
renewLoggerSet LoggerSet{..} = case lgrsetFilePath of
    Nothing -> return ()
    Just file -> do
        newfd <- openFileFD file
        oldfd <- atomicModifyIORef' lgrsetFdRef (\fd -> (newfd, fd))
        closeFD oldfd

-- | Flushing the buffers, closing the internal file information
--   and freeing the buffers.
rmLoggerSet :: LoggerSet -> IO ()
rmLoggerSet LoggerSet{..} = do
    fd <- readIORef lgrsetFdRef
    when (isFDValid fd) $ do
        case lgrsetLogger of
            SL sl -> stopLoggers sl
            ML ml -> stopLoggers ml
        when (isJust lgrsetFilePath) $ closeFD fd
        writeIORef lgrsetFdRef invalidFD

-- | Replacing the file path in 'LoggerSet' and returning a new
--   'LoggerSet' and the old file path.
replaceLoggerSet :: LoggerSet -> FilePath -> (LoggerSet, Maybe FilePath)
replaceLoggerSet lgrset@LoggerSet{..} new_file_path =
    (lgrset{lgrsetFilePath = Just new_file_path}, lgrsetFilePath)
