{-# LANGUAGE CPP #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}

-- | This module provides a fast logging system which
--   scales on multicore environments (i.e. +RTS -N\<x\>).
--
--   Note: This library does not guarantee correct ordering of log messages
--   when program is run on more than one core thus users
--   should rely more on message timestamps than on their order in the
--   log.
module System.Log.FastLogger (
    -- * FastLogger
    FastLogger,
    LogType,
    LogType' (..),
    newFastLogger,
    newFastLogger1,
    withFastLogger,

    -- * Timed FastLogger
    TimedFastLogger,
    newTimedFastLogger,
    withTimedFastLogger,

    -- * Log messages
    LogStr,
    ToLogStr (..),
    fromLogStr,
    logStrLength,

    -- * Buffer size
    BufSize,
    defaultBufSize,

    -- * LoggerSet
    module System.Log.FastLogger.LoggerSet,

    -- * Date cache
    module System.Log.FastLogger.Date,

    -- * File rotation
    module System.Log.FastLogger.File,

    -- * Types
    module System.Log.FastLogger.Types,
) where

import Control.Concurrent (MVar, newMVar, putMVar, tryTakeMVar)
import Control.Exception (SomeException (..), bracket, handle)
import System.EasyFile (getFileSize)

import System.Log.FastLogger.Date
import System.Log.FastLogger.File
import System.Log.FastLogger.IO
import System.Log.FastLogger.Imports
import System.Log.FastLogger.LogStr
import System.Log.FastLogger.LoggerSet
import System.Log.FastLogger.Types

----------------------------------------------------------------

-- | 'FastLogger' simply log 'logStr'.
type FastLogger = LogStr -> IO ()

-- | 'TimedFastLogger' pass 'FormattedTime' to callback and simply log its result.
-- this can be used to customize how to log timestamp.
--
-- Usually, one would write a wrapper on top of 'TimedFastLogger', for example:
--
-- > {-# LANGUAGE OverloadedStrings #-}
-- >
-- > log :: TimedFastLogger -> LogStr -> IO ()
-- > log logger msg = logger (\time -> toLogStr (show time) <> " " <> msg <> "\n")
type TimedFastLogger = (FormattedTime -> LogStr) -> IO ()

type LogType = LogType' LogStr

-- | Logger Type.
data LogType' a where
    LogNone :: LogType' LogStr
        -- ^ No logging.
    LogStdout :: BufSize
        -> LogType' LogStr
        -- ^ Logging to stdout.
        --   'BufSize' is a buffer size
        --   for each capability.
    LogStderr :: BufSize
        -> LogType' LogStr
        -- ^ Logging to stderr.
        --   'BufSize' is a buffer size
        --   for each capability.
    LogFileNoRotate :: FilePath
        -> BufSize
        -> LogType' LogStr
        -- ^ Logging to a file.
        --   'BufSize' is a buffer size
        --   for each capability.
    LogFile :: FileLogSpec
        -> BufSize
        -> LogType' LogStr
        -- ^ Logging to a file.
        --   'BufSize' is a buffer size
        --   for each capability.
        --   File rotation is done on-demand.
    LogFileTimedRotate :: TimedFileLogSpec
        -> BufSize
        -> LogType' LogStr
        -- ^ Logging to a file.
        --   'BufSize' is a buffer size
        --   for each capability.
        --   Rotation happens based on check specified
        --   in 'TimedFileLogSpec'.
    LogCallback :: (v -> IO ())
        -> IO ()
        -> LogType' v
        -- ^ Logging with a log and flush action.
        -- run flush after log each message.

-- | Initialize a 'FastLogger' without attaching timestamp
-- a tuple of logger and clean up action are returned.
-- This type signature should be read as:
--
-- > newFastLogger :: LogType -> IO (FastLogger, IO ())
--
-- This logger uses `numCapabilities` many buffers, and thus
-- does not provide time-ordered output.
-- For time-ordered output, use `newFastLogger1`.
newFastLogger :: LogType' v -> IO (v -> IO (), IO ())
newFastLogger typ = newFastLoggerCore Nothing typ

-- | Like `newFastLogger`, but creating a logger that uses only 1
-- internal builder. This scales less on multi-core machines and
-- consumes more memory because of an internal queue but provides
-- time-ordered output.
newFastLogger1 :: LogType' v -> IO (v -> IO (), IO ())
newFastLogger1 typ = newFastLoggerCore (Just 1) typ

newFastLoggerCore :: Maybe Int -> LogType' v -> IO (v -> IO (), IO ())
newFastLoggerCore mn typ = case typ of
    LogNone -> return (const noOp, noOp)
    LogStdout bsize -> newStdoutLoggerSetN bsize mn >>= stdLoggerInit
    LogStderr bsize -> newStderrLoggerSetN bsize mn >>= stdLoggerInit
    LogFileNoRotate fp bsize -> newFileLoggerSetN bsize mn fp >>= fileLoggerInit
    LogFile fspec bsize -> rotateLoggerInit fspec bsize
    LogFileTimedRotate fspec bsize -> timedRotateLoggerInit fspec bsize
    LogCallback cb flush -> return (\str -> cb str >> flush, noOp)
  where
    stdLoggerInit lgrset = return (pushLogStr lgrset, rmLoggerSet lgrset)
    fileLoggerInit lgrset = return (pushLogStr lgrset, rmLoggerSet lgrset)
    rotateLoggerInit fspec bsize = do
        lgrset <- newFileLoggerSetN bsize mn $ log_file fspec
        ref <- newIORef (0 :: Int)
        mvar <- newMVar ()
        let logger str = do
                cnt <- decrease ref
                pushLogStr lgrset str
                when (cnt <= 0) $ tryRotate lgrset fspec ref mvar
        return (logger, rmLoggerSet lgrset)
    timedRotateLoggerInit fspec bsize = do
        cache <- newTimeCache $ timed_timefmt fspec
        now <- cache
        lgrset <- newFileLoggerSetN bsize mn $ prefixTime now $ timed_log_file fspec
        ref <- newIORef now
        mvar <- newMVar lgrset
        let logger str = do
                ct <- cache
                updated <- updateTime (timed_same_timeframe fspec) ref ct
                when updated $ tryTimedRotate fspec ct mvar
                pushLogStr lgrset str
        return (logger, rmLoggerSet lgrset)

-- | 'bracket' version of 'newFastLogger'
withFastLogger :: LogType -> (FastLogger -> IO a) -> IO a
withFastLogger typ log' = bracket (newFastLogger typ) snd (log' . fst)

-- | Initialize a 'FastLogger' with timestamp attached to each message.
-- a tuple of logger and clean up action are returned.
newTimedFastLogger
    :: IO FormattedTime
    -- ^ How do we get 'FormattedTime'?
    -- "System.Log.FastLogger.Date" provide cached formatted time.
    -> LogType
    -> IO (TimedFastLogger, IO ())
newTimedFastLogger tgetter typ = case typ of
    LogNone -> return (const noOp, noOp)
    LogStdout bsize -> newStdoutLoggerSet bsize >>= stdLoggerInit
    LogStderr bsize -> newStderrLoggerSet bsize >>= stdLoggerInit
    LogFileNoRotate fp bsize -> newFileLoggerSet bsize fp >>= fileLoggerInit
    LogFile fspec bsize -> rotateLoggerInit fspec bsize
    LogFileTimedRotate fspec bsize -> timedRotateLoggerInit fspec bsize
    LogCallback cb flush -> return (\f -> tgetter >>= cb . f >> flush, noOp)
  where
    stdLoggerInit lgrset = return (\f -> tgetter >>= pushLogStr lgrset . f, rmLoggerSet lgrset)
    fileLoggerInit lgrset = return (\f -> tgetter >>= pushLogStr lgrset . f, rmLoggerSet lgrset)
    rotateLoggerInit fspec bsize = do
        lgrset <- newFileLoggerSet bsize $ log_file fspec
        ref <- newIORef (0 :: Int)
        mvar <- newMVar ()
        let logger f = do
                cnt <- decrease ref
                t <- tgetter
                pushLogStr lgrset (f t)
                when (cnt <= 0) $ tryRotate lgrset fspec ref mvar
        return (logger, rmLoggerSet lgrset)
    timedRotateLoggerInit fspec bsize = do
        cache <- newTimeCache $ timed_timefmt fspec
        now <- cache
        lgrset <- newFileLoggerSet bsize $ prefixTime now $ timed_log_file fspec
        ref <- newIORef now
        mvar <- newMVar lgrset
        let logger f = do
                ct <- cache
                updated <- updateTime (timed_same_timeframe fspec) ref ct
                when updated $ tryTimedRotate fspec ct mvar
                t <- tgetter
                pushLogStr lgrset (f t)
        return (logger, rmLoggerSet lgrset)

-- | 'bracket' version of 'newTimeFastLogger'
withTimedFastLogger
    :: IO FormattedTime -> LogType -> (TimedFastLogger -> IO a) -> IO a
withTimedFastLogger tgetter typ log' = bracket (newTimedFastLogger tgetter typ) snd (log' . fst)

----------------------------------------------------------------

noOp :: IO ()
noOp = return ()

decrease :: IORef Int -> IO Int
decrease ref = atomicModifyIORef' ref (\x -> (x - 1, x - 1))

-- updateTime returns whether the timeframe has changed
updateTime
    :: (FormattedTime -> FormattedTime -> Bool)
    -> IORef FormattedTime
    -> FormattedTime
    -> IO Bool
updateTime cmp ref newTime = atomicModifyIORef' ref (\x -> (newTime, not $ cmp x newTime))

tryRotate :: LoggerSet -> FileLogSpec -> IORef Int -> MVar () -> IO ()
tryRotate lgrset spec ref mvar = bracket lock unlock rotateFiles
  where
    lock = tryTakeMVar mvar
    unlock Nothing = return ()
    unlock _ = putMVar mvar ()
    rotateFiles Nothing = return ()
    rotateFiles _ = do
        msiz <- getSize
        case msiz of
            -- A file is not available.
            -- So, let's set a big value to the counter so that
            -- this function is not called frequently.
            Nothing -> writeIORef ref 1000000
            Just siz
                | siz > limit -> do
                    rotate spec
                    renewLoggerSet lgrset
                    writeIORef ref $ estimate limit
                | otherwise ->
                    writeIORef ref $ estimate (limit - siz)
    file = log_file spec
    limit = log_file_size spec
    getSize =
        handle (\(SomeException _) -> return Nothing) $
            -- The log file is locked by GHC.
            -- We need to get its file size by the way not using locks.
            Just . fromIntegral <$> getFileSize file
    -- 200 is an ad-hoc value for the length of log line.
    estimate x = fromInteger (x `div` 200)

tryTimedRotate :: TimedFileLogSpec -> FormattedTime -> MVar LoggerSet -> IO ()
tryTimedRotate spec now mvar = bracket lock unlock rotateFiles
  where
    lock = tryTakeMVar mvar
    unlock Nothing = return ()
    unlock (Just lgrset) = do
        let (newlgrset, current_path) = replaceLoggerSet lgrset new_file_path
        putMVar mvar newlgrset
        case current_path of
            Nothing -> return ()
            Just path -> timed_post_process spec path
    rotateFiles Nothing = return ()
    rotateFiles (Just lgrset) = do
        let (newlgrset, _) = replaceLoggerSet lgrset new_file_path
        renewLoggerSet newlgrset
    new_file_path = prefixTime now $ timed_log_file spec
