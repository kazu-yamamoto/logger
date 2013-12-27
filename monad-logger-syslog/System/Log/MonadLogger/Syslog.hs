{-# LANGUAGE OverloadedStrings #-}
module System.Log.MonadLogger.Syslog where

import Control.Monad.IO.Class (MonadIO ())
import Control.Monad.Logger
import System.Posix.Syslog
import Data.Text (unpack)
import System.Log.FastLogger (LogStr, fromLogStr)
import qualified Data.ByteString.Char8 as BS8

runSyslogLoggingT :: MonadIO m => LoggingT m a -> m a
runSyslogLoggingT = (`runLoggingT` syslogOutput)

-- TODO: useSyslog allows giving a source name and should be more efficient
-- But it assumes IO
-- Perhaps should use mmorph to generalize IO to MonadIO
{-
runSyslogLoggingT :: MonadIO m => String -> LoggingT m a -> m a
runSyslogLoggingT source action =
  useSyslog source (runLoggingT action syslogOutput)
-}


syslogOutput :: Loc
              -> LogSource
              -> LogLevel
              -> LogStr
              -> IO ()
syslogOutput l s level msg =
    syslog (levelToPriority level) $
        BS8.unpack $ fromLogStr $
            defaultLogStr l s level msg


levelToPriority :: LogLevel -> Priority
levelToPriority LevelDebug = Debug
levelToPriority LevelInfo  = Info
levelToPriority LevelWarn  = Warning
levelToPriority LevelError = Error
levelToPriority (LevelOther level) =
    case level of
        "Emergency" -> Emergency
        "Alert"     -> Alert
        "Critical"  -> Critical
        "Notice"    -> Notice
        _ -> error $ "unknown log level: " ++ unpack level
