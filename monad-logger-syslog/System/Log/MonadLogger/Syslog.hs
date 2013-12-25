{-# LANGUAGE OverloadedStrings #-}
module System.Log.MonadLogger.Syslog where

import Control.Monad.IO.Class (MonadIO ())
import Control.Monad.Logger
import System.Posix.Syslog
import Data.Text (unpack)
import System.Log.FastLogger (LogStr, logStrBuilder)
import qualified Data.ByteString.Char8 as BS8
import Blaze.ByteString.Builder as BB

runSyslogLoggingT :: MonadIO m => LoggingT m a -> m a
runSyslogLoggingT = (`runLoggingT` syslogOutput)


syslogOutput :: Loc
              -> LogSource
              -> LogLevel
              -> LogStr
              -> IO ()
syslogOutput l s level msg =
    syslog (levelToPriority level) $
        BS8.unpack $
        toByteString $
        logStrBuilder $
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
