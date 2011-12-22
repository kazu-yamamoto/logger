module Network.Wai.Logger.Prefork (
    logCheck
  , logInit
  , logController
  , ApacheLogger
  , LogType(..)
  , IPAddrSource(..)
  , FileLogSpec(..)
  , LogController
  ) where

import Control.Concurrent
import Control.Monad
import Network.Wai.Logger
import Network.Wai.Logger.Prefork.File
import Network.Wai.Logger.Prefork.Stdout
import Network.Wai.Logger.Prefork.Types
import System.Log.FastLogger

logCheck :: LogType -> IO ()
logCheck LogNone   = return ()
logCheck LogStdout = return ()
logCheck (LogFile spec) = check spec

logInit :: IPAddrSource -> LogType -> IO ApacheLogger
logInit _ LogNone            = noLoggerInit
logInit ipsrc LogStdout      = stdoutLoggerInit ipsrc
logInit ipsrc (LogFile spec) = fileLoggerInit ipsrc spec

noLoggerInit :: IO ApacheLogger
noLoggerInit = return noLogger
  where
    noLogger _ _ _ = return ()

logController :: LogType -> LogController
logController LogNone        = noLoggerController
logController LogStdout      = noLoggerController
logController (LogFile spec) = fileLoggerController spec

noLoggerController :: LogController
noLoggerController _ = forever $ threadDelay 10000000
