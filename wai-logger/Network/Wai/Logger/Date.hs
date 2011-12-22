{-# LANGUAGE DoAndIfThenElse, BangPatterns #-}

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
import System.Posix (EpochTime, epochTime)
import Data.Time.Clock.POSIX

-- | A type for zoned date.
type ZonedDate = ByteString

data DateCache = DateCache {
    unixTime :: !EpochTime
  , zonedDate :: !ZonedDate
  }

-- | Reference to the 'ZonedDate' cache.
newtype DateRef = DateRef (IORef DateCache)

-- | Getting 'ZonedDate' from the 'ZonedDate' cache.
getDate :: DateRef -> IO ZonedDate
getDate (DateRef ref) = do
    newEt <- epochTime
    cache <- readIORef ref
    let oldEt = unixTime cache
    if oldEt == newEt then
        return $ zonedDate cache
    else do
        newCache <- newDate newEt
        !_ <- atomicModifyIORef ref (\_ -> (newCache, ()))
        return $ zonedDate newCache

newDate :: EpochTime -> IO DateCache
newDate et = DateCache et . format <$> toZonedTime et
  where
    toZonedTime = utcToLocalZonedTime . toUTC
    toUTC = posixSecondsToUTCTime . realToFrac
    format = BS.pack . formatTime defaultTimeLocale "%d/%b/%Y:%T %z"

-- | Initializing the 'ZonedDate' cache.
dateInit :: IO DateRef
dateInit = DateRef <$> (epochTime >>= newDate >>= newIORef)
