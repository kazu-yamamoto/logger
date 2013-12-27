{-# LANGUAGE OverloadedStrings #-}
import Control.Monad.Logger
import System.Log.MonadLogger.Syslog
import Shelly

main :: IO ()
main = do
    let logFile = "/var/log/syslog"
    runSyslogLoggingT $ logDebugN "HELLO!"
    shellyNoDir $
        whenM (test_e logFile) $
            cmd "tail" logFile
