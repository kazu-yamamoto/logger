{-# LANGUAGE DoAndIfThenElse, BangPatterns #-}
{-# LANGUAGE CPP #-}

module Network.Wai.Logger.Date (
    ZonedDate
  , DateRef
  , dateInit
  , getDate
  ) where

import Control.Applicative
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as BS
import Data.IORef
import Data.Time
import System.Locale
#if WINDOWS
#define TIME UTCTime
#define GETTIME getCurrentTime
#else
import System.Posix (EpochTime, epochTime)
import Data.Time.Clock.POSIX
#define TIME EpochTime
#define GETTIME epochTime
#endif

-- | A type for zoned date.
type ZonedDate = ByteString

data DateCache = DateCache {
    unixTime :: !TIME
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
newDate et = DateCache et . format <$> toZonedTime et
  where
    toZonedTime = utcToLocalZonedTime . toUTC
#if WINDOWS
    toUTC = id
#else
    toUTC = posixSecondsToUTCTime . realToFrac
#endif
    format = BS.pack . formatTime defaultTimeLocale "%d/%b/%Y:%T %z"

-- | Initializing the 'ZonedDate' cache.
dateInit :: IO DateRef
dateInit = DateRef <$> (GETTIME >>= newDate >>= newIORef)
