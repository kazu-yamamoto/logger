{-# LANGUAGE CPP, OverloadedStrings #-}

module System.Log.FastLogger.Date (
    ZonedDate
  , zonedDateCacheConf
  ) where

import Data.ByteString
import System.Date.Cache
#if WINDOWS
import qualified Data.ByteString.Char8 as BS
import Data.Time
import System.Locale
#else
import Data.UnixTime
import System.Posix (EpochTime, epochTime)
#endif

-- | A type for zoned date.
type ZonedDate = ByteString

#if WINDOWS
zonedDateCacheConf :: DateCacheConf UTCTime
zonedDateCacheConf = DateCacheConf {
    getTime = getCurrentTime
  , formatDate = \ut -> do
      zt <- utcToLocalZonedTime ut
      return $ BS.pack $ formatTime defaultTimeLocale "%d/%b/%Y:%T %z" zt
  }
#else
zonedDateCacheConf :: DateCacheConf EpochTime
zonedDateCacheConf = DateCacheConf {
    getTime = epochTime
  , formatDate = return . formatUnixTime "%d/%b/%Y:%T %z" . fromEpochTime
  }
#endif
