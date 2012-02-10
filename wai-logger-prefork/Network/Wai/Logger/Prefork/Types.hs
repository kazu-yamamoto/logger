module Network.Wai.Logger.Prefork.Types (
    FileLogSpec(..)
  , LogType(..)
  , LogController
  ) where

import System.Log.FastLogger
import System.Posix (ProcessID)

data LogType = LogNone | LogStdout | LogFile FileLogSpec

type LogController = [ProcessID] -> IO ()
