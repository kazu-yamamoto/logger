{-# LANGUAGE RecordWildCards #-}

module System.Log.FastLogger.MultiLogger (
    MultiLogger(..)
  , newMultiLogger
  ) where


import Control.Concurrent (myThreadId, threadCapability)
import Data.Array (Array, listArray, (!), bounds)

import System.Log.FastLogger.Imports
import System.Log.FastLogger.LogStr
import System.Log.FastLogger.Write

----------------------------------------------------------------

newtype MLogger = MLogger {
    lgrRef :: IORef LogStr
  }

newtype MultiLogger = MultiLogger {
    mlgrArray :: Array Int MLogger
  }

instance Loggers MultiLogger where
    stopLoggers = System.Log.FastLogger.MultiLogger.flushAllLog
    pushAllLog  = System.Log.FastLogger.MultiLogger.pushAllLog
    flushAllLog = System.Log.FastLogger.MultiLogger.flushAllLog

----------------------------------------------------------------

newMLogger :: IO MLogger
newMLogger = MLogger <$> newIORef mempty

newMultiLogger :: Int -> IO MultiLogger
newMultiLogger n = MultiLogger . listArray (0,n-1) <$> replicateM n newMLogger

----------------------------------------------------------------

pushAllLog :: MultiLogger -> BufFD -> LogStr -> IO ()
pushAllLog MultiLogger{..} buffd logmsg = do
    (i, _) <- myThreadId >>= threadCapability
    -- The number of capability could be dynamically changed.
    -- So, let's check the upper boundary of the array.
    let u = snd $ bounds mlgrArray
        lim = u + 1
        j | i < lim   = i
          | otherwise = i `mod` lim
    let logger = mlgrArray ! j
    pushLog logger buffd logmsg

pushLog :: MLogger -> BufFD -> LogStr -> IO ()
pushLog logger@MLogger{..} buffd@BufFD{..} nlogmsg@(LogStr nlen _)
  | nlen > buffdBufSize = do
      flushLog logger buffd
      -- Make sure we have a large enough buffer to hold the entire
      -- contents, thereby allowing for a single write system call and
      -- avoiding interleaving. This does not address the possibility
      -- of write not writing the entire buffer at once.
      writeBigLogStr buffd nlogmsg
  | otherwise = do
    action <- atomicModifyIORef' lgrRef checkBuf
    action
  where
    checkBuf ologmsg@(LogStr olen _)
      | buffdBufSize < olen + nlen = (nlogmsg, writeLogStr buffd ologmsg)
      | otherwise                  = (ologmsg <> nlogmsg, return ())

----------------------------------------------------------------

flushAllLog :: MultiLogger -> BufFD -> IO ()
flushAllLog MultiLogger{..} buffd = do
    let flushIt i = flushLog (mlgrArray ! i) buffd
        (l,u) = bounds mlgrArray
        nums = [l .. u]
    mapM_ flushIt nums

flushLog :: MLogger -> BufFD -> IO ()
flushLog MLogger{..} buffd = do
    -- If a special buffer is prepared for flusher, this MVar could
    -- be removed. But such a code does not contribute logging speed
    -- according to experiment. And even with the special buffer,
    -- there is no grantee that this function is exclusively called
    -- for a buffer. So, we use MVar here.
    -- This is safe and speed penalty can be ignored.
    action <- atomicModifyIORef' lgrRef (\old -> (mempty, writeLogStr buffd old))
    action
