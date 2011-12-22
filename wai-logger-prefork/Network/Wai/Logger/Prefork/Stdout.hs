module Network.Wai.Logger.Prefork.Stdout (stdoutLoggerInit) where

import Control.Applicative
import qualified Data.ByteString as BS
import Data.ByteString.Char8 (pack)
import Network.Wai.Logger
import Network.Wai.Logger.Prefork.Types
import System.Log.FastLogger

stdoutLoggerInit :: IPAddrSource -> IO ApacheLogger
stdoutLoggerInit ipsrc = stdoutLogger ipsrc <$> dateInit

stdoutLogger :: IPAddrSource -> DateRef -> ApacheLogger
stdoutLogger ipsrc dateref req status msiz = do
    date <- getDate dateref
    putToStdout $ logmsg date
  where
    putToStdout = BS.putStr . BS.concat . map toBS
    logmsg date = apacheFormat ipsrc date req status msiz
    toBS (LS s) = pack s
    toBS (LB s) = s
