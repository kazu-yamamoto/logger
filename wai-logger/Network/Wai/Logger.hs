module Network.Wai.Logger (
  -- * High level functions
    ApacheLogger
  , withStdoutLogger
  -- * Creating a logger
  , initLogger
  -- * Types
  , IPAddrSource(..)
  , LogType(..)
  , FileLogSpec(..)
  , LogFlusher
  , LogRotator
  -- * Date cacher
  , clockDateCacher
  , ZonedDate
  , DateCacheGetter
  , DateCacheUpdater
  -- * Utilities
  , logCheck
  ) where

import Control.Applicative ((<$>))
import Control.Concurrent (forkIO, threadDelay, killThread)
import Control.Exception (handle, SomeException(..), bracket)
import Control.Monad (when, void)
import Network.HTTP.Types
import Network.Wai
import System.Log.FastLogger
import System.Posix.Files (getFileStatus, fileSize)
import System.Posix.IO

import Network.Wai.Logger.Apache
import Network.Wai.Logger.Date

----------------------------------------------------------------

-- | Executing a function which takes 'ApacheLogger'.
--   This 'ApacheLogger' writes log message to stdout.
--   Each buffer (4K bytes) is flushed every second.
withStdoutLogger :: (ApacheLogger -> IO a) -> IO a
withStdoutLogger app = bracket setup teardown $ \(aplogger, _, _) ->
    app aplogger
  where
    setup = do
        (getter, updater) <- clockDateCacher
        (aplogger, flusher, _) <- initLogger FromFallback (LogStdout 4096) getter
        t <- forkIO $ do
            threadDelay 1000000
            updater
            flusher
        return (aplogger, flusher, t)
    teardown (_, flusher, t) = do
        void $ flusher -- why type is not inferred?
        killThread t

----------------------------------------------------------------

-- | Apache style logger.
type ApacheLogger = Request -> Status -> Maybe Integer -> IO ()

-- | Flushing log messages in the buffers.
--   This is explicitly called from your program.
--   Probably, one second and 10 seconds is proper to stdout and
--   log files, respectively.
--   See the source code of 'withStdoutLogger'.
type LogFlusher = IO ()
-- | Rotating log files.
--   This is explicitly called from your program.
--   Probably, 10 seconds is proper.
type LogRotator = IO ()

-- | Logger Type.
data LogType = LogNone                     -- ^ No logging.
             | LogStdout BufSize           -- ^ Logging to stdout.
                                           --   'BufSize' is a buffer size
                                           --   for each capability.
             | LogFile FileLogSpec BufSize -- ^ Logging to a file.
                                           --   'BufSize' is a buffer size
                                           --   for each capability.

----------------------------------------------------------------

-- |
-- Creating 'ApacheLogger' according to 'LogType'.
initLogger :: IPAddrSource -> LogType -> DateCacheGetter -> IO (ApacheLogger, LogFlusher, LogRotator)
initLogger _     LogNone             _       = noLoggerInit
initLogger ipsrc (LogStdout size)    dateget = stdoutLoggerInit ipsrc size dateget
initLogger ipsrc (LogFile spec size) dateget = fileLoggerInit ipsrc spec size dateget

----------------------------------------------------------------

noLoggerInit :: IO (ApacheLogger, LogFlusher, LogRotator)
noLoggerInit = return $! (noLogger, noFlusher, noRotator)
  where
    noLogger _ _ _ = return ()
    noFlusher = return ()
    noRotator = return ()

stdoutLoggerInit :: IPAddrSource -> BufSize -> DateCacheGetter
                 -> IO (ApacheLogger, LogFlusher, LogRotator)
stdoutLoggerInit ipsrc size dateget = do
    lgrset <- newLoggerSet stdOutput size
    let logger = apache lgrset ipsrc dateget
        flusher = flushLogMsg lgrset
        noRotater = return ()
    return $! (logger, flusher, noRotater)

fileLoggerInit :: IPAddrSource -> FileLogSpec -> BufSize -> DateCacheGetter
               -> IO (ApacheLogger, LogFlusher, LogRotator)
fileLoggerInit ipsrc spec size dateget = do
    fd <- logOpen (log_file spec)
    lgrset <- newLoggerSet fd size
    let logger = apache lgrset ipsrc dateget
        flusher = flushLogMsg lgrset
        rotator = logRotater lgrset spec
    return $! (logger, flusher, rotator)

----------------------------------------------------------------

apache :: LoggerSet -> IPAddrSource -> DateCacheGetter -> ApacheLogger
apache lgrset ipsrc dateget req st mlen = do
    zdata <- dateget
    pushLogMsg lgrset (apacheLogMsg ipsrc zdata req st mlen)

----------------------------------------------------------------

logRotater :: LoggerSet -> FileLogSpec -> IO ()
logRotater lgrset spec = do
    over <- isOver
    when over $ do
        rotate spec
        logOpen (log_file spec) >>= renewLoggerSet lgrset
  where
    file = log_file spec
    isOver = handle (\(SomeException _) -> return False) $ do
        siz <- fromIntegral . fileSize <$> getFileStatus file
        if siz > log_file_size spec then
            return True
          else
            return False

----------------------------------------------------------------

-- |
-- Checking if a log file can be written if 'LogType' is 'LogFile'.
logCheck :: LogType -> IO ()
logCheck LogNone          = return ()
logCheck (LogStdout _)    = return ()
logCheck (LogFile spec _) = check spec
