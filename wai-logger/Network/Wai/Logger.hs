{-# LANGUAGE OverloadedStrings #-}

-- | Logging system for WAI applications.
--
-- High level sample code:
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
-- > import System.IO
-- >
-- > main :: IO ()
-- > main = do
-- >     logger <- stdoutApacheLoggerInit FromSocket
-- >     run 3000 $ logapp logger
-- >
-- > logapp :: ApacheLogger -> Application
-- > logapp logger req = do
-- >     let status = status200
-- >         len = 4
-- >     liftIO $ logger req status (Just len)
-- >     liftIO $ hFlush stdout
-- >     return $ ResponseBuilder status
-- >         [("Content-Type", "text/plain")
-- >         ,("Content-Length", pack (show len))]
-- >         $ fromByteString "PONG"
--
-- Low level sample code:
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
-- > import System.IO
-- > import System.Log.FastLogger
-- >
-- > main :: IO ()
-- > main = do
-- >     dref <- dateInit
-- >     run 3000 $ logapp dref
-- >
-- > logapp :: DateRef -> Application
-- > logapp dref req = do
-- >     date <- liftIO $ getDate dref
-- >     let status = status200
-- >         len = 4
-- >     liftIO $ hPutLogStr stdout $ apacheFormat FromSocket date req status (Just len)
-- >     liftIO $ hFlush stdout
-- >     return $ ResponseBuilder status
-- >         [("Content-Type", "text/plain")
-- >         ,("Content-Length", pack (show len))]
-- >         $ fromByteString "PONG"

module Network.Wai.Logger (
    ApacheLogger
  , stdoutApacheLoggerInit
  , module Network.Wai.Logger.Format
  , module Network.Wai.Logger.Utils
  , module System.Log.FastLogger.Date
  ) where

import Control.Applicative
import Network.HTTP.Types
import Network.Wai
import Network.Wai.Logger.Format
import Network.Wai.Logger.Utils
import System.IO
import System.Log.FastLogger
import System.Log.FastLogger.Date

-- | Apache style logger for WAI
type ApacheLogger = Request -> Status -> Maybe Integer -> IO ()

-- | Obtaining Apache style logger to stdout
stdoutApacheLoggerInit :: IPAddrSource -> IO ApacheLogger
stdoutApacheLoggerInit ipsrc = stdoutLogger ipsrc <$> dateInit

stdoutLogger :: IPAddrSource -> DateRef -> ApacheLogger
stdoutLogger ipsrc dateref req status msiz =
    getDate dateref >>= hPutLogStr stdout . logmsg
  where
    logmsg date = apacheFormat ipsrc date req status msiz

