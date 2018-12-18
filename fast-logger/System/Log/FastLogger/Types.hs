module System.Log.FastLogger.Types (
  -- * Types
    TimeFormat
  , FormattedTime
  ) where

import Data.ByteString (ByteString)

----------------------------------------------------------------
  
-- | Type aliaes for date format and formatted date.
type FormattedTime = ByteString
type TimeFormat = ByteString
