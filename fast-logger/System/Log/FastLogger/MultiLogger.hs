{-# LANGUAGE RecordWildCards #-}

module System.Log.FastLogger.MultiLogger (
    MultiLogger,
    newMultiLogger,
) where

import Control.Concurrent (
    MVar,
    myThreadId,
    newMVar,
    takeMVar,
    threadCapability,
    withMVar,
 )
import Data.Array (Array, bounds, listArray, (!))

import System.Log.FastLogger.FileIO
import System.Log.FastLogger.IO
import System.Log.FastLogger.Imports
import System.Log.FastLogger.LogStr
import System.Log.FastLogger.Write

----------------------------------------------------------------

newtype MLogger = MLogger
    { lgrRef :: IORef LogStr
    }

-- | A scale but non-time-ordered logger.
data MultiLogger = MultiLogger
    { mlgrArray :: Array Int MLogger
    , mlgrMBuffer :: MVar Buffer
    , mlgrBufSize :: BufSize
    , mlgrFdRef :: IORef FD
    }

instance Loggers MultiLogger where
    stopLoggers = System.Log.FastLogger.MultiLogger.stopLoggers
    pushLog = System.Log.FastLogger.MultiLogger.pushLog
    flushAllLog = System.Log.FastLogger.MultiLogger.flushAllLog

----------------------------------------------------------------

newMLogger :: IO MLogger
newMLogger = MLogger <$> newIORef mempty

-- | Creating `MultiLogger`.
--   The first argument is the number of the internal builders.
newMultiLogger :: Int -> BufSize -> IORef FD -> IO MultiLogger
newMultiLogger n bufsize fdref = do
    mbuf <- getBuffer bufsize >>= newMVar
    arr <- listArray (0, n - 1) <$> replicateM n newMLogger
    return $
        MultiLogger
            { mlgrArray = arr
            , mlgrMBuffer = mbuf
            , mlgrBufSize = bufsize
            , mlgrFdRef = fdref
            }

----------------------------------------------------------------

pushLog :: MultiLogger -> LogStr -> IO ()
pushLog ml@MultiLogger{..} logmsg = do
    (i, _) <- myThreadId >>= threadCapability
    -- The number of capability could be dynamically changed.
    -- So, let's check the upper boundary of the array.
    let u = snd $ bounds mlgrArray
        lim = u + 1
        j
            | i < lim = i
            | otherwise = i `mod` lim
    let logger = mlgrArray ! j
    pushLog' logger logmsg
  where
    pushLog' logger@MLogger{..} nlogmsg@(LogStr nlen _)
        | nlen > mlgrBufSize = do
            flushLog ml logger
            -- Make sure we have a large enough buffer to hold the entire
            -- contents, thereby allowing for a single write system call and
            -- avoiding interleaving. This does not address the possibility
            -- of write not writing the entire buffer at once.
            writeBigLogStr' ml nlogmsg
        | otherwise = do
            action <- atomicModifyIORef' lgrRef checkBuf
            action
      where
        checkBuf ologmsg@(LogStr olen _)
            | mlgrBufSize < olen + nlen = (nlogmsg, writeLogStr' ml ologmsg)
            | otherwise = (ologmsg <> nlogmsg, return ())

----------------------------------------------------------------

flushAllLog :: MultiLogger -> IO ()
flushAllLog ml@MultiLogger{..} = do
    let flushIt i = flushLog ml (mlgrArray ! i)
        (l, u) = bounds mlgrArray
        nums = [l .. u]
    mapM_ flushIt nums

flushLog :: MultiLogger -> MLogger -> IO ()
flushLog ml MLogger{..} = do
    -- If a special buffer is prepared for flusher, this MVar could
    -- be removed. But such a code does not contribute logging speed
    -- according to experiment. And even with the special buffer,
    -- there is no grantee that this function is exclusively called
    -- for a buffer. So, we use MVar here.
    -- This is safe and speed penalty can be ignored.
    old <- atomicModifyIORef' lgrRef (\old -> (mempty, old))
    writeLogStr' ml old

----------------------------------------------------------------

stopLoggers :: MultiLogger -> IO ()
stopLoggers ml@MultiLogger{..} = do
    System.Log.FastLogger.MultiLogger.flushAllLog ml
    takeMVar mlgrMBuffer >>= freeBuffer

----------------------------------------------------------------

writeLogStr' :: MultiLogger -> LogStr -> IO ()
writeLogStr' MultiLogger{..} logstr =
    withMVar mlgrMBuffer $ \buf -> writeLogStr buf mlgrFdRef logstr

writeBigLogStr' :: MultiLogger -> LogStr -> IO ()
writeBigLogStr' MultiLogger{..} logstr =
    withMVar mlgrMBuffer $ \_ -> writeBigLogStr mlgrFdRef logstr
