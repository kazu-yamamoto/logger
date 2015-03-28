{-# LANGUAGE CPP, OverloadedStrings #-}

-- |
-- Formatting time is slow.
-- This package provides mechanisms to cache formatted date.
module Network.Wai.Logger.Date (
  -- * Types
    DateCacheGetter
  , DateCacheUpdater
  , ZonedDate
  -- * Cache configuration
  , DateCacheConf(..)
  , zonedDateCacheConf
  -- * Date cacher
  , clockDateCacher
  ) where

import Control.AutoUpdate (mkAutoUpdate, defaultUpdateSettings, updateAction)
import Data.ByteString (ByteString)
#if WINDOWS
import qualified Data.ByteString.Char8 as BS
import Data.Time (UTCTime, formatTime, getCurrentTime, utcToLocalZonedTime)
# if MIN_VERSION_time(1,5,0)
import Data.Time (defaultTimeLocale)
# else
import System.Locale (defaultTimeLocale)
# endif
#else
import Data.UnixTime (formatUnixTime, fromEpochTime)
import System.Posix (EpochTime, epochTime)
#endif

----------------------------------------------------------------

-- | Getting cached 'ZonedDate'.
type DateCacheGetter = IO ZonedDate
-- | Updateing cached 'ZonedDate'. This should be called every second.
--   See the source code of 'withStdoutLogger'.
type DateCacheUpdater = IO ()

----------------------------------------------------------------

-- | A type for zoned date.
type ZonedDate = ByteString

----------------------------------------------------------------

data DateCacheConf t = DateCacheConf {
    -- | A function to get a time. E.g 'epochTime' and 'getCurrentTime'.
    getTime :: IO t
    -- | A function to format a time.
  , formatDate :: t -> IO ByteString
  }

#if WINDOWS
-- | Zoned date cacher using UTC.
zonedDateCacheConf :: DateCacheConf UTCTime
zonedDateCacheConf = DateCacheConf {
    getTime = getCurrentTime
  , formatDate = \ut -> do
      zt <- utcToLocalZonedTime ut
      return $ BS.pack $ formatTime defaultTimeLocale "%d/%b/%Y:%T %z" zt
  }
#else
-- | Zoned date cacher using UnixTime.
zonedDateCacheConf :: DateCacheConf EpochTime
zonedDateCacheConf = DateCacheConf {
    getTime = epochTime
  , formatDate = formatUnixTime "%d/%b/%Y:%T %z" . fromEpochTime
  }
#endif

----------------------------------------------------------------

-- |
-- Returning 'DateCacheGetter' and 'DateCacheUpdater'.
--
-- Note: Since version 2.1.2, this function uses the auto-update package
-- internally, and therefore the @DateCacheUpdater@ value returned need
-- not be called. To wit, the return value is in fact an empty action.

clockDateCacher :: IO (DateCacheGetter, DateCacheUpdater)
clockDateCacher = do
    getter <- mkAutoUpdate defaultUpdateSettings
        { updateAction = getTime zonedDateCacheConf
                     >>= formatDate zonedDateCacheConf
        }
    return (getter, return ())
