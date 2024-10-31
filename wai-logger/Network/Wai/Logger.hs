{-# LANGUAGE CPP #-}
{-# LANGUAGE GADTs #-}

-- | Apache style logger for WAI applications.
--
-- An example:
--
-- > {-# LANGUAGE OverloadedStrings #-}
-- > module Main where
-- >
-- > import Data.ByteString.Builder (byteString)
-- > import Control.Monad.IO.Class (liftIO)
-- > import qualified Data.ByteString.Char8 as BS
-- > import Network.HTTP.Types (status200)
-- > import Network.Wai (Application, responseBuilder)
-- > import Network.Wai.Handler.Warp (run)
-- > import Network.Wai.Logger (withStdoutLogger, ApacheLogger)
-- >
-- > main :: IO ()
-- > main = withStdoutLogger $ \aplogger ->
-- >     run 3000 $ logApp aplogger
-- >
-- > logApp :: ApacheLogger -> Application
-- > logApp aplogger req response = do
-- >     liftIO $ aplogger req status (Just len)
-- >     response $ responseBuilder status hdr msg
-- >   where
-- >     status = status200
-- >     hdr = [("Content-Type", "text/plain")]
-- >     pong = "PONG"
-- >     msg = byteString pong
-- >     len = fromIntegral $ BS.length pong
module Network.Wai.Logger (
    -- * High level functions
    ApacheLogger,
    withStdoutLogger,
    ServerPushLogger,

    -- * Creating a logger
    ApacheLoggerActions,
    apacheLogger,
    serverpushLogger,
    logRotator,
    logRemover,
    initLoggerUser,
    initLogger,

    -- * Types
    IPAddrSource (..),
    LogType' (..),
    LogType,
    FileLogSpec (..),

    -- * Utilities
    showSockAddr,
    logCheck,

    -- * Backward compability
    clockDateCacher,
    ZonedDate,
    DateCacheGetter,
    DateCacheUpdater,
) where

#if __GLASGOW_HASKELL__ < 709
import Control.Applicative ((<$>))
#endif
import Control.Exception (bracket)
import Control.Monad (void)
import Data.ByteString (ByteString)
import Network.HTTP.Types (Status)
import Network.Wai (Request)
import System.Log.FastLogger

import Network.Wai.Logger.Apache
import Network.Wai.Logger.IP (showSockAddr)

----------------------------------------------------------------

-- | Executing a function which takes 'ApacheLogger'.
--   This 'ApacheLogger' writes log message to stdout.
--   Each buffer (4K bytes) is flushed every second.
withStdoutLogger :: (ApacheLogger -> IO a) -> IO a
withStdoutLogger app = bracket setup teardown $ \(aplogger, _) ->
    app aplogger
  where
    setup = do
        tgetter <- newTimeCache simpleTimeFormat
        apf <- initLogger FromFallback (LogStdout 4096) tgetter
        let aplogger = apacheLogger apf
            remover = logRemover apf
        return (aplogger, remover)
    teardown (_, remover) = void remover

----------------------------------------------------------------

-- | Apache style logger.
type ApacheLogger = Request -> Status -> Maybe Integer -> IO ()

-- | HTTP/2 server push logger in Apache style.
type ServerPushLogger = Request -> ByteString -> Integer -> IO ()

-- | Function set of Apache style logger.
data ApacheLoggerActions = ApacheLoggerActions
    { apacheLogger :: ApacheLogger
    -- ^ The Apache logger.
    , serverpushLogger :: ServerPushLogger
    -- ^ The HTTP/2 server push logger.
    , logRotator :: IO ()
    -- ^ This is obsoleted. Rotation is done on-demand.
    --   So, this is now an empty action.
    , logRemover :: IO ()
    -- ^ Removing resources relating to Apache logger.
    --   E.g. flushing and deallocating internal buffers.
    }

----------------------------------------------------------------

-- | Creating 'ApacheLogger' according to 'LogType'.
initLoggerUser
    :: ToLogStr user
    => Maybe (Request -> Maybe user)
    -> IPAddrSource
    -> LogType
    -> IO FormattedTime
    -> IO ApacheLoggerActions
initLoggerUser ugetter ipsrc typ tgetter = do
    (fl, cleanUp) <- newFastLogger typ
    return $
        ApacheLoggerActions
            { apacheLogger = apache fl ipsrc ugetter tgetter
            , serverpushLogger = serverpush fl ipsrc ugetter tgetter
            , logRotator = return ()
            , logRemover = cleanUp
            }

initLogger
    :: IPAddrSource
    -> LogType
    -> IO FormattedTime
    -> IO ApacheLoggerActions
initLogger = initLoggerUser nouser
  where
    nouser :: Maybe (Request -> Maybe ByteString)
    nouser = Nothing

--- | Checking if a log file can be written if 'LogType' is 'LogFileNoRotate' or 'LogFile'.
logCheck :: LogType -> IO ()
logCheck LogNone = return ()
logCheck (LogStdout _) = return ()
logCheck (LogStderr _) = return ()
logCheck (LogFileNoRotate fp _) = check fp
logCheck (LogFile spec _) = check (log_file spec)
logCheck (LogFileTimedRotate spec _) = check (timed_log_file spec)
logCheck (LogCallback _ _) = return ()

----------------------------------------------------------------

apache
    :: ToLogStr user
    => (LogStr -> IO ())
    -> IPAddrSource
    -> Maybe (Request -> Maybe user)
    -> IO FormattedTime
    -> ApacheLogger
apache cb ipsrc userget dateget req st mlen = do
    zdata <- dateget
    cb (apacheLogStr ipsrc (justGetUser userget) zdata req st mlen)

serverpush
    :: ToLogStr user
    => (LogStr -> IO ())
    -> IPAddrSource
    -> Maybe (Request -> Maybe user)
    -> IO FormattedTime
    -> ServerPushLogger
serverpush cb ipsrc userget dateget req path size = do
    zdata <- dateget
    cb (serverpushLogStr ipsrc (justGetUser userget) zdata req path size)

---------------------------------------------------------------

-- | Getting cached 'ZonedDate'.
type DateCacheGetter = IO ZonedDate

-- | Updateing cached 'ZonedDate'. This should be called every second.
--   See the source code of 'withStdoutLogger'.
type DateCacheUpdater = IO ()

-- | A type for zoned date.
type ZonedDate = FormattedTime

-- |
-- Returning 'DateCacheGetter' and 'DateCacheUpdater'.
--
-- Note: Since version 2.1.2, this function uses the auto-update package
-- internally, and therefore the @DateCacheUpdater@ value returned need
-- not be called. To wit, the return value is in fact an empty action.
clockDateCacher :: IO (DateCacheGetter, DateCacheUpdater)
clockDateCacher = do
    tgetter <- newTimeCache simpleTimeFormat
    return (tgetter, return ())

justGetUser :: Maybe (Request -> Maybe user) -> (Request -> Maybe user)
justGetUser (Just getter) = getter
justGetUser Nothing = \_ -> Nothing
