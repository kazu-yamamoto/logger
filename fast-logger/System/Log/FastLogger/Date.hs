{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}

-- |
-- Formatting time is slow.
-- This package provides mechanisms to cache formatted date.
module System.Log.FastLogger.Date
  ( -- * Date cacher
    newTimeCache
  , simpleTimeFormat
  , simpleTimeFormat'
  ) where

import Control.AutoUpdate (mkAutoUpdate, defaultUpdateSettings, updateAction)
import System.Log.FastLogger.Types (TimeFormat, FormattedTime)
import Data.UnixTime (formatUnixTime, fromEpochTime)
import System.PosixCompat.Types (EpochTime)
import System.PosixCompat.Time (epochTime)

----------------------------------------------------------------

-- | Get date using UnixTime.
getTime :: IO EpochTime
getTime = epochTime
-- | Format unix EpochTime date.
formatDate :: TimeFormat -> EpochTime -> IO FormattedTime
formatDate fmt = formatUnixTime fmt . fromEpochTime

----------------------------------------------------------------

-- |  Make 'IO' action which get cached formatted local time.
-- Use this to avoid the cost of frequently time formatting by caching an
-- auto updating formatted time, this cache update every 1 second.
-- more detail in "Control.AutoUpdate"
newTimeCache :: TimeFormat -> IO (IO FormattedTime)
newTimeCache fmt = mkAutoUpdate defaultUpdateSettings{
        updateAction = getTime >>= formatDate fmt
    }

-- | A simple time cache using format @"%d/%b/%Y:%T %z"@
simpleTimeFormat :: TimeFormat
simpleTimeFormat = "%d/%b/%Y:%T %z"

-- | A simple time cache using format @"%d-%b-%Y %T"@
simpleTimeFormat' :: TimeFormat
simpleTimeFormat' = "%d-%b-%Y %T"
