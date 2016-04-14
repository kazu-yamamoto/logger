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
  -- * Date cacher
  , clockDateCacher
  , ZonedDate
  , DateCacheGetter
  , DateCacheUpdater
  -- * Utilities
  , logCheck
  , showSockAddr
  ) where

import Control.Applicative ((<$>))
import Control.AutoUpdate (mkAutoUpdate, defaultUpdateSettings, updateAction)
import Control.Concurrent (MVar, newMVar, tryTakeMVar, putMVar)
import Control.Exception (handle, SomeException(..), bracket)
import Control.Monad (when, void)
import Network.HTTP.Types (Status)
import Network.Wai (Request)
import System.EasyFile (getFileSize)
import System.Log.FastLogger

import Network.Wai.Logger.Apache
import Network.Wai.Logger.Date
import Network.Wai.Logger.IORef
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
        (getter, _updater) <- clockDateCacher
        apf <- initLogger FromFallback (LogStdout 4096) getter
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

-- |
-- Creating 'ApacheLogger' according to 'LogType'.
initLogger :: IPAddrSource -> LogType -> DateCacheGetter
           -> IO ApacheLoggerActions
initLogger _     LogNone             _       = noLoggerInit
initLogger ipsrc (LogStdout size)    dateget = newStdoutLoggerSet size >>= stdLoggerInit ipsrc dateget
initLogger ipsrc (LogStderr size)    dateget = newStderrLoggerSet size >>= stdLoggerInit ipsrc dateget
initLogger ipsrc (LogFile fp size)   dateget = newFileLoggerSet size fp >>= stdLoggerInit ipsrc dateget
initLogger ipsrc (LogFileAutoRotate spec size) dateget = fileLoggerInit ipsrc spec size dateget
initLogger ipsrc (LogCallback cb flush) dateget = callbackLoggerInit ipsrc cb flush dateget

----------------------------------------------------------------

noLoggerInit :: IO ApacheLoggerActions
noLoggerInit = return ApacheLoggerActions {
    apacheLogger = noLogger
  , logRotator = noRotator
  , logRemover = noRemover
  }
  where
    noLogger _ _ _ = return ()
    noRotator = return ()
    noRemover = return ()

stdLoggerInit :: IPAddrSource -> DateCacheGetter
                 -> LoggerSet -> IO ApacheLoggerActions
stdLoggerInit ipsrc dateget lgrset = do
    let logger = apache (pushLogStr lgrset) ipsrc dateget
        noRotator = return ()
        remover = rmLoggerSet lgrset
    return ApacheLoggerActions {
        apacheLogger = logger
      , logRotator = noRotator
      , logRemover = remover
      }

fileLoggerInit :: IPAddrSource -> FileLogSpec -> BufSize -> DateCacheGetter
               -> IO ApacheLoggerActions
fileLoggerInit ipsrc spec size dateget = do
    lgrset <- newFileLoggerSet size $ log_file spec
    ref <- newIORef (0 :: Int)
    mvar <- newMVar ()
    let logger a b c = do
            cnt <- decrease ref
            apache (pushLogStr lgrset) ipsrc dateget a b c
            when (cnt <= 0) $ tryRotate lgrset spec ref mvar
        noRotator = return ()
        remover = rmLoggerSet lgrset
    return ApacheLoggerActions {
        apacheLogger = logger
      , logRotator = noRotator
      , logRemover = remover
      }

decrease :: IORef Int -> IO Int
decrease ref = atomicModifyIORef' ref (\x -> (x - 1, x - 1))

callbackLoggerInit :: IPAddrSource -> (LogStr -> IO ()) -> IO () -> DateCacheGetter
                   -> IO ApacheLoggerActions
callbackLoggerInit ipsrc cb flush dateget = do
    flush' <- mkAutoUpdate defaultUpdateSettings
        { updateAction = flush
        }
    let logger a b c = apache cb ipsrc dateget a b c >> flush'
        noRotator = return ()
        remover = return ()
    return ApacheLoggerActions {
        apacheLogger = logger
      , logRotator = noRotator
      , logRemover = remover
      }

----------------------------------------------------------------

apache :: (LogStr -> IO ()) -> IPAddrSource -> DateCacheGetter -> ApacheLogger
apache cb ipsrc dateget req st mlen = do
    zdata <- dateget
    cb (apacheLogStr ipsrc zdata req st mlen)

----------------------------------------------------------------

tryRotate :: LoggerSet -> FileLogSpec -> IORef Int -> MVar () -> IO ()
tryRotate lgrset spec ref mvar = bracket lock unlock rotateFiles
  where
    lock           = tryTakeMVar mvar
    unlock Nothing = return ()
    unlock _       = putMVar mvar ()
    rotateFiles Nothing = return ()
    rotateFiles _       = do
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
                | otherwise -> do
                    writeIORef ref $ estimate (limit - siz)
    file = log_file spec
    limit = log_file_size spec
    getSize = handle (\(SomeException _) -> return Nothing) $ do
        -- The log file is locked by GHC.
        -- We need to get its file size by the way not using locks.
        Just . fromIntegral <$> getFileSize file
    -- 200 is an ad-hoc value for the length of log line.
    estimate x = fromInteger (x `div` 200)

----------------------------------------------------------------

-- |
-- Checking if a log file can be written if 'LogType' is 'LogFile'.
logCheck :: LogType -> IO ()
logCheck LogNone          = return ()
logCheck (LogStdout _)    = return ()
logCheck (LogStderr _)    = return ()
logCheck (LogFile fp _)   = check fp
logCheck (LogFileAutoRotate spec _) = check (log_file spec)
logCheck (LogCallback _ _) = return ()
