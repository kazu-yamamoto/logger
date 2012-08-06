{-# LANGUAGE CPP #-}

module System.Log.FastLogger.Date (
    ZonedDate
  , DateRef
  , dateInit
  , getDate
  ) where

import Control.Applicative
import Data.ByteString (ByteString)
import Data.IORef
#if WINDOWS
import qualified Data.ByteString.Char8 as BS
import Data.Time
import System.Locale
#define TIME UTCTime
#define GETTIME getCurrentTime
#else
import Data.UnixTime
import System.Posix (EpochTime, epochTime)
#define TIME EpochTime
#define GETTIME epochTime
#endif

-- | A type for zoned date.
type ZonedDate = ByteString

data DateCache = DateCache {
#if WINDOWS
    unixTime :: !UTCTime
#else
    unixTime :: !EpochTime
#endif
  , zonedDate :: !ZonedDate
  }

-- | Reference to the 'ZonedDate' cache.
newtype DateRef = DateRef (IORef DateCache)

-- | Getting 'ZonedDate' from the 'ZonedDate' cache.
getDate :: DateRef -> IO ZonedDate
getDate (DateRef ref) = do
    newEt <- GETTIME
    cache <- readIORef ref
    let oldEt = unixTime cache
    if oldEt == newEt then
        return $ zonedDate cache
      else do
        newCache <- newDate newEt
        writeIORef ref newCache
        return $ zonedDate newCache

newDate :: TIME -> IO DateCache
#if WINDOWS
newDate et = DateCache et . format <$> toZonedTime et
  where
    toZonedTime = utcToLocalZonedTime
    format = BS.pack . formatTime defaultTimeLocale "%d/%b/%Y:%T %z"
#else
newDate et = return $ DateCache et zdate
  where
    zdate = formatUnixTimeGMT webDateFormat $ fromEpochTime et
#endif

-- | Initializing the 'ZonedDate' cache.
dateInit :: IO DateRef
dateInit = DateRef <$> (GETTIME >>= newDate >>= newIORef)
