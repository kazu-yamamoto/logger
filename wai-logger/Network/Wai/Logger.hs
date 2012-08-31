{-# LANGUAGE OverloadedStrings #-}

-- | Logging system for WAI applications.
--
-- Sample code:
--
-- > {-# LANGUAGE OverloadedStrings #-}
-- > module Main where
-- >
-- > import Blaze.ByteString.Builder (fromByteString)
-- > import Control.Monad.IO.Class (liftIO)
-- > import Data.ByteString.Char8
-- > import Network.HTTP.Types (status200)
-- > import Network.Wai
-- > import Network.Wai.Handler.Warp
-- > import Network.Wai.Logger
-- >
-- > main :: IO ()
-- > main = do
-- >     aplogger <- stdoutApacheLoggerInit FromSocket True
-- >     run 3000 $ logapp aplogger
-- >
-- > logapp :: ApacheLogger -> Application
-- > logapp aplogger req = do
-- >     let status = status200
-- >         len = 4
-- >     liftIO $ aplogger req status (Just len)
-- >     return $ ResponseBuilder status
-- >         [("Content-Type", "text/plain")
-- >         ,("Content-Length", pack (show len))]
-- >         $ fromByteString "PONG"

module Network.Wai.Logger (
    ApacheLogger
  , stdoutApacheLoggerInit
  , stdoutApacheLoggerInit2
  , module Network.Wai.Logger.Format
  , module Network.Wai.Logger.Utils
  ) where

import Control.Applicative
import Network.HTTP.Types
import Network.Wai
import Network.Wai.Logger.Format
import Network.Wai.Logger.Utils
import System.Date.Cache
import System.IO
import System.Log.FastLogger

-- | Apache style logger for WAI
type ApacheLogger = Request -> Status -> Maybe Integer -> IO ()

-- | Obtaining Apache style logger to stdout
stdoutApacheLoggerInit :: IPAddrSource
                       -> Bool -- ^ Automatically flush on each logging?
                       -> IO ApacheLogger
stdoutApacheLoggerInit ipsrc autoFlash =
    stdoutLogger ipsrc <$> mkLogger autoFlash stdout

-- | Obtaining Apache style logger to stdout
stdoutApacheLoggerInit2 :: IPAddrSource
                        -> Bool -- ^ Automatically flush on each logging?
                        -> (DateCacheGetter, DateCacheCloser)
                        -> IO ApacheLogger
stdoutApacheLoggerInit2 ipsrc autoFlash dc =
    stdoutLogger ipsrc <$> mkLogger2 autoFlash stdout dc

stdoutLogger :: IPAddrSource -> Logger -> ApacheLogger
stdoutLogger ipsrc logger req status msiz = do
    date <- loggerDate logger
    loggerPutStr logger $ logmsg date
  where
    logmsg date = apacheFormat ipsrc date req status msiz
