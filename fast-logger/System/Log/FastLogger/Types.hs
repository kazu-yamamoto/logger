module System.Log.FastLogger.Types (
  -- * Types
    TimeFormat
  , FormattedTime
  ) where

import System.Log.FastLogger.Internal.Imports

----------------------------------------------------------------

-- | Type aliaes for date format and formatted date.
type FormattedTime = ByteString
type TimeFormat = ByteString
