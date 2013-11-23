module Network.Wai.Logger (
  -- * Types
    IPAddrSource(..)
  , LogType(..)
  , ApacheLogger
  , LogFlusher
  , LogRotator
  , DateCacheGetter
  , DateCacheUpdater
  , ZonedDate
  -- * Utilities
  , clockDateCacher
  , initLogger
  , logCheck
  ) where

import Control.Applicative ((<$>))
import Control.Exception (handle,SomeException(..))
import Control.Monad (when)
import Network.HTTP.Types
import Network.Wai
import System.Log.FastLogger
import System.Posix.Files (getFileStatus, fileSize)
import System.Posix.IO

import Network.Wai.Logger.Apache
import Network.Wai.Logger.Date

----------------------------------------------------------------

type ApacheLogger = Request -> Status -> Maybe Integer -> IO ()

type LogFlusher = IO ()
type LogRotator = IO ()

data LogType = LogNone
             | LogStdout BufSize
             | LogFile FileLogSpec BufSize

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
