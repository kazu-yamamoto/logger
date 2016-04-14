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
  -- * Creating a logger
  , ApacheLoggerActions(..)
  , initLogger
  -- * Types
  , IPAddrSource(..)
  , LogType(..)
  , FileLogSpec(..)
  -- * Utilities
  , showSockAddr
  , logCheck
  -- * Backward compability
  , ZonedDate
  , clockDateCacher
  ) where

import Control.Applicative ((<$>))
import Control.Exception (bracket)
import Control.Monad (void)
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
        apf <- initLogger FromFallback (LogStdout 4096) simpleTimeCache
        let aplogger = apacheLogger apf
            remover = logRemover apf
        return (aplogger, remover)
    teardown (_, remover) = void remover

----------------------------------------------------------------

-- | Apache style logger.
type ApacheLogger = Request -> Status -> Maybe Integer -> IO ()

data ApacheLoggerActions = ApacheLoggerActions {
    apacheLogger :: ApacheLogger
    -- | This is obsoleted. Rotation is done on-demand.
    --   So, this is now an empty action.
  , logRotator :: IO ()
    -- | Removing resources relating Apache logger.
    --   E.g. flushing and deallocating internal buffers.
  , logRemover :: IO ()
  }

----------------------------------------------------------------

-- | Creating 'ApacheLogger' according to 'LogType'.
initLogger :: IPAddrSource -> LogType -> (IO FormattedTime)
           -> IO ApacheLoggerActions
initLogger ipsrc typ tgetter = do
    (fl, cleanUp) <- newFastLogger typ
    return $ ApacheLoggerActions (apache fl ipsrc tgetter) (return ()) cleanUp

--- | Checking if a log file can be written if 'LogType' is 'LogFile'.
logCheck :: LogType -> IO ()
logCheck LogNone          = return ()
logCheck (LogStdout _)    = return ()
logCheck (LogStderr _)    = return ()
logCheck (LogFile fp _)   = check fp
logCheck (LogFileAutoRotate spec _) = check (log_file spec)
logCheck (LogCallback _ _) = return ()

----------------------------------------------------------------

apache :: (LogStr -> IO ()) -> IPAddrSource -> (IO FormattedTime) -> ApacheLogger
apache cb ipsrc dateget req st mlen = do
    zdata <- dateget
    cb (apacheLogStr ipsrc zdata req st mlen)

---------------------------------------------------------------

type ZonedDate = FormattedTime

clockDateCacher :: IO (IO ZonedDate, IO ())
clockDateCacher = return (simpleTimeCache, return ())
