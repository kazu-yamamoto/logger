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

import Control.Applicative ((<$>))
import Data.ByteString (ByteString)
import Data.IORef (newIORef, readIORef, writeIORef)
#if WINDOWS
import qualified Data.ByteString.Char8 as BS
import Data.Time
import System.Locale
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

data DateCache t = DateCache {
    timeKey :: !t
  , formattedDate :: !ByteString
  } deriving (Eq, Show)

----------------------------------------------------------------

newDate :: DateCacheConf t -> t -> IO (DateCache t)
newDate setting tm = DateCache tm <$> formatDate setting tm

-- |
-- Returning 'DateCacheGetter' and 'DateCacheUpdater'.

clockDateCacher :: IO (DateCacheGetter, DateCacheUpdater)
clockDateCacher = do
    ref <- getTime zonedDateCacheConf >>= newDate zonedDateCacheConf >>= newIORef
    return $! (getter ref, clock ref)
  where
    getter ref = formattedDate <$> readIORef ref
    clock ref = do
        tm <- getTime zonedDateCacheConf
        date <- formatDate zonedDateCacheConf tm
        let new = DateCache {
                timeKey = tm
              , formattedDate = date
              }
        writeIORef ref new
