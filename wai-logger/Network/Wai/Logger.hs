module Network.Wai.Logger (
  -- * High level functions
    ApacheLogger
  , withStdoutLogger
  -- * Creating a logger
  , ApacheLoggerFunctions(..)
  , initLogger
  -- * Types
  , IPAddrSource(..)
  , LogType(..)
  , FileLogSpec(..)
  -- * Date cacher
  , clockDateCacher
  , ZonedDate
  , DateCacheGetter
  , DateCacheUpdater
  -- * Utilities
  , logCheck
  ) where

import Control.Concurrent (forkIO, threadDelay, killThread)
import Control.Exception (handle, SomeException(..), bracket)
import Control.Monad (when, void)
import GHC.IO.FD (stdout)
import Network.HTTP.Types (Status)
import Network.Wai (Request)
import System.IO (withFile, hFileSize, IOMode(..))
import System.Log.FastLogger

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
        apf <- initLogger FromFallback (LogStdout 4096) getter
        let aplogger = apacheLogger apf
            flusher = logFlusher apf
            remover = logRemover apf
        t <- forkIO $ do
            threadDelay 1000000
            updater
            flusher
        return (aplogger, remover, t)
    teardown (_, remover, t) = do
        void remover
        killThread t

----------------------------------------------------------------

-- | Apache style logger.
type ApacheLogger = Request -> Status -> Maybe Integer -> IO ()

data ApacheLoggerFunctions = ApacheLoggerFunctions {
    apacheLogger :: ApacheLogger
    -- | Flushing log messages in the buffers.
    --   This is explicitly called from your program.
    --   Probably, one second and 10 seconds is proper to stdout and
    --   log files, respectively.
    --   See the source code of 'withStdoutLogger'.
  , logFlusher :: IO ()
    -- | Rotating log files.
    --   This is explicitly called from your program.
    --   Probably, 10 seconds is proper.
  , logRotator :: IO ()
    -- | Removing resources relating Apache logger.
  , logRemover :: IO ()
  }

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
initLogger :: IPAddrSource -> LogType -> DateCacheGetter
           -> IO ApacheLoggerFunctions
initLogger _     LogNone             _       = noLoggerInit
initLogger ipsrc (LogStdout size)    dateget = stdoutLoggerInit ipsrc size dateget
initLogger ipsrc (LogFile spec size) dateget = fileLoggerInit ipsrc spec size dateget

----------------------------------------------------------------

noLoggerInit :: IO ApacheLoggerFunctions
noLoggerInit = return ApacheLoggerFunctions {
    apacheLogger = noLogger
  , logFlusher = noFlusher
  , logRotator = noRotator
  , logRemover = noRemover
  }
  where
    noLogger _ _ _ = return ()
    noFlusher = return ()
    noRotator = return ()
    noRemover = return ()

stdoutLoggerInit :: IPAddrSource -> BufSize -> DateCacheGetter
                 -> IO ApacheLoggerFunctions
stdoutLoggerInit ipsrc size dateget = do
    lgrset <- newLoggerSet size stdout
    let logger = apache lgrset ipsrc dateget
        flusher = flushLogMsg lgrset
        noRotator = return ()
        remover = rmLoggerSet lgrset
    return ApacheLoggerFunctions {
        apacheLogger = logger
      , logFlusher = flusher
      , logRotator = noRotator
      , logRemover = remover
      }

fileLoggerInit :: IPAddrSource -> FileLogSpec -> BufSize -> DateCacheGetter
               -> IO ApacheLoggerFunctions
fileLoggerInit ipsrc spec size dateget = do
    fd <- logOpen (log_file spec)
    lgrset <- newLoggerSet size fd
    let logger = apache lgrset ipsrc dateget
        flusher = flushLogMsg lgrset
        rotator = logRotater lgrset spec
        remover = rmLoggerSet lgrset
    return ApacheLoggerFunctions {
        apacheLogger = logger
      , logFlusher = flusher
      , logRotator = rotator
      , logRemover = remover
      }

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
        siz <- withFile file ReadMode hFileSize
        return (siz > log_file_size spec)

----------------------------------------------------------------

-- |
-- Checking if a log file can be written if 'LogType' is 'LogFile'.
logCheck :: LogType -> IO ()
logCheck LogNone          = return ()
logCheck (LogStdout _)    = return ()
logCheck (LogFile spec _) = check spec
