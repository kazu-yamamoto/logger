-- |
-- Formatting time is slow.
-- This package provides mechanisms to cache formatted date.
module System.Date.Cache (
    -- * Types
    DateCacheConf (..),
    DateCacheGetter,
    DateCacheCloser,

    -- * Date cacher
    ondemandDateCacher,
    clockDateCacher,
) where

import Control.Applicative
import Control.Concurrent
import Data.ByteString (ByteString)
import Data.IORef

type DateCacheGetter = IO ByteString
type DateCacheCloser = IO ()

data DateCache t = DateCache
    { timeKey :: !t
    , formattedDate :: !ByteString
    }
    deriving (Eq, Show)

data DateCacheConf t = DateCacheConf
    { getTime :: IO t
    -- ^ A function to get a time. E.g 'epochTime' and 'getCurrentTime'.
    , formatDate :: t -> IO ByteString
    -- ^ A function to format a time.
    }

newDate :: DateCacheConf t -> t -> IO (DateCache t)
newDate setting tm = DateCache tm <$> formatDate setting tm

-- |
-- Date cacher which gets a time and formatted it only when
-- returned getter is executed.
ondemandDateCacher
    :: Eq t => DateCacheConf t -> IO (DateCacheGetter, DateCacheCloser)
ondemandDateCacher setting = do
    ref <- getTime setting >>= newDate setting >>= newIORef
    return (getter ref, closer)
  where
    getter ref = do
        newTm <- getTime setting
        cache <- readIORef ref
        let oldTm = timeKey cache
        if oldTm == newTm
            then
                return $ formattedDate cache
            else do
                newCache <- newDate setting newTm
                writeIORef ref newCache
                return $ formattedDate newCache
    closer = return ()

-- |
-- Date cacher which gets a time and formatted it every second.
-- This returns a getter.
clockDateCacher
    :: Eq t => DateCacheConf t -> IO (DateCacheGetter, DateCacheCloser)
clockDateCacher setting = do
    ref <- getTime setting >>= newDate setting >>= newIORef
    tid <- forkIO $ clock ref
    return (getter ref, closer tid)
  where
    getter ref = formattedDate <$> readIORef ref
    clock ref = do
        threadDelay 1000000
        tm <- getTime setting
        date <- formatDate setting tm
        let new =
                DateCache
                    { timeKey = tm
                    , formattedDate = date
                    }
        writeIORef ref new
        clock ref
    closer tid = killThread tid
