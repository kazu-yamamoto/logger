{-# LANGUAGE CPP #-}

-- | Apache style logger for WAI applications.
--
-- An example:
--
-- > {-# LANGUAGE OverloadedStrings #-}
-- > module Main where
-- >
-- > import Blaze.ByteString.Builder (fromByteString)
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
-- >     msg = fromByteString pong
-- >     len = fromIntegral $ BS.length pong

module Network.Wai.Logger (
  -- * High level functions
    ApacheLogger
  , withStdoutLogger
  , ServerPushLogger
  -- * Creating a logger
  , ApacheLoggerActions
  , apacheLogger
  , serverpushLogger
  , logRotator
  , logRemover
  , initLogger
  -- * Types
  , IPAddrSource(..)
  , LogType(..)
  , FileLogSpec(..)
  -- * Utilities
  , showSockAddr
  , logCheck
  -- * Backward compability
  , clockDateCacher
  , ZonedDate
  , DateCacheGetter
  , DateCacheUpdater
  ) where

#if __GLASGOW_HASKELL__ < 709
import Control.Applicative ((<$>))
#endif
import Control.Exception (bracket)
import Control.Monad (void)
import Data.ByteString (ByteString)
import Network.HTTP.Types (Status)
import Network.Socket (SockAddr)
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
type ServerPushLogger = SockAddr -> ByteString -> Integer -> ByteString -> Maybe ByteString -> IO ()

-- | Function set of Apache style logger.
data ApacheLoggerActions = ApacheLoggerActions {
    -- | The Apache logger.
    apacheLogger :: ApacheLogger
    -- | The HTTP/2 server push logger.
  , serverpushLogger :: ServerPushLogger
    -- | This is obsoleted. Rotation is done on-demand.
    --   So, this is now an empty action.
  , logRotator :: IO ()
    -- | Removing resources relating to Apache logger.
    --   E.g. flushing and deallocating internal buffers.
  , logRemover :: IO ()
  }

----------------------------------------------------------------

-- | Creating 'ApacheLogger' according to 'LogType'.
initLogger :: IPAddrSource -> LogType -> IO FormattedTime
           -> IO ApacheLoggerActions
initLogger ipsrc typ tgetter = do
    (fl, cleanUp) <- newFastLogger typ
    return $ ApacheLoggerActions {
        apacheLogger     = apache fl ipsrc tgetter
      , serverpushLogger = serverpush fl tgetter
      , logRotator       = return ()
      , logRemover       = cleanUp
      }

--- | Checking if a log file can be written if 'LogType' is 'LogFileNoRotate' or 'LogFile'.
logCheck :: LogType -> IO ()
logCheck LogNone          = return ()
logCheck (LogStdout _)    = return ()
logCheck (LogStderr _)    = return ()
logCheck (LogFileNoRotate fp _)  = check fp
logCheck (LogFile spec _)        = check (log_file spec)
logCheck (LogCallback _ _) = return ()

----------------------------------------------------------------

apache :: (LogStr -> IO ()) -> IPAddrSource -> IO FormattedTime -> ApacheLogger
apache cb ipsrc dateget req st mlen = do
    zdata <- dateget
    cb (apacheLogStr ipsrc zdata req st mlen)

serverpush :: (LogStr -> IO ()) -> IO FormattedTime -> ServerPushLogger
serverpush cb dateget sa path size ref mua = do
    zdata <- dateget
    cb (serverpushLogStr zdata sa path size ref mua)

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
