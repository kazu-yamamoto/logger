-- |
-- Formatting time is slow.
-- This package provides mechanisms to cache formatted date
module System.Date.Cache (
    Setting(..)
  , ondemandDateCacher
  , clockDateCacher
    ) where

import Control.Applicative
import Control.Concurrent
import Control.Monad
import Data.ByteString (ByteString)
import Data.IORef

data DateCache t = DateCache {
    timeKey   :: !t
  , formattedDate :: !ByteString
  } deriving (Eq, Show)

data Setting t = Setting {
    -- | A function to get a time. E.g 'epochTime' and 'getCurrentTime'.
    getTime :: IO t
    -- | A function to format a time.
  , formatDate :: t -> ByteString
  }

newDate :: Setting t -> t -> IO (DateCache t)
newDate setting tm = return $ DateCache tm (formatDate setting tm)

-- |
-- Date cacher which gets a time and formatted it only when
-- returned getter is executed.
ondemandDateCacher :: Eq t => Setting t -> IO (IO ByteString)
ondemandDateCacher setting = getter <$> (getTime setting >>= newDate setting >>= newIORef)
  where
    getter ref = do
        newTm <- getTime setting
        cache <- readIORef ref
        let oldTm = timeKey cache
        if oldTm == newTm then
            return $ formattedDate cache
          else do
            newCache <- newDate setting newTm
            writeIORef ref newCache
            return $ formattedDate newCache

-- |
-- Date cacher which gets a time and formatted it every second.
-- This returns a getter.
clockDateCacher :: Eq t => Setting t -> IO (IO ByteString)
clockDateCacher setting = do
    ref <- getTime setting >>= newDate setting >>= newIORef
    void . forkIO $ clock ref
    return $ getter ref
  where
    getter ref = formattedDate <$> readIORef ref
    clock ref = do
        threadDelay 1000000
        tm <- getTime setting
        let new = DateCache {
                timeKey = tm
              , formattedDate = formatDate setting tm
              }
        writeIORef ref new
        clock ref
