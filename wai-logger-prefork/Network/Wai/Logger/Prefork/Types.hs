module Network.Wai.Logger.Prefork.Types (
    FileLogSpec (..),
    LogType (..),
    LogController,
) where

import System.Log.FastLogger
import System.Posix (ProcessID, Signal)

data LogType
    = LogNone
    | LogStdout
    | -- | 'Signal' is used to tell child processes to reopen a log file.
      LogFile FileLogSpec Signal

type LogController = [ProcessID] -> IO ()
