module Network.Wai.Logger.Prefork.Types (
    ApacheLogger
  , FileLogSpec(..)
  , LogType(..)
  , LogController
  ) where

import Network.HTTP.Types
import Network.Wai
import System.Log.FastLogger
import System.Posix (ProcessID)

data LogType = LogNone | LogStdout | LogFile FileLogSpec

type ApacheLogger = Request -> Status -> Maybe Integer -> IO ()

type LogController = [ProcessID] -> IO ()
