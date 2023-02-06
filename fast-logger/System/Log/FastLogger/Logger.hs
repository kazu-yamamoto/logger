{-# LANGUAGE CPP #-}
{-# LANGUAGE Safe #-}

module System.Log.FastLogger.Logger (
    Logger(..)
  , newLogger
  , pushLog
  , flushLog
  ) where


import Control.Concurrent (MVar, withMVar)
import Foreign.Marshal.Alloc (allocaBytes)
import Foreign.Ptr (plusPtr)

import System.Log.FastLogger.FileIO
import System.Log.FastLogger.IO
import System.Log.FastLogger.Imports
import System.Log.FastLogger.LogStr

----------------------------------------------------------------

newtype Logger = Logger (IORef LogStr)

----------------------------------------------------------------

newLogger :: IO Logger
newLogger = Logger <$> newIORef mempty

----------------------------------------------------------------

pushLog :: IORef FD -> BufSize -> MVar Buffer -> Logger -> LogStr -> IO ()
pushLog fdref size mbuf logger@(Logger ref) nlogmsg@(LogStr nlen nbuilder)
  | nlen > size = do
      flushLog fdref size mbuf logger
      -- Make sure we have a large enough buffer to hold the entire
      -- contents, thereby allowing for a single write system call and
      -- avoiding interleaving. This does not address the possibility
      -- of write not writing the entire buffer at once.
      allocaBytes nlen $ \buf -> withMVar mbuf $ \_ ->
        toBufIOWith buf nlen (write fdref) nbuilder
  | otherwise = do
    action <- atomicModifyIORef' ref checkBuf
    action
  where
    push msg = withMVar mbuf $ \buf -> writeLogStr fdref buf size msg
    checkBuf ologmsg@(LogStr olen _)
      | size < olen + nlen = (nlogmsg,            push ologmsg)
      | otherwise          = (ologmsg <> nlogmsg, return ())

----------------------------------------------------------------

flushLog :: IORef FD -> BufSize -> MVar Buffer -> Logger -> IO ()
flushLog fdref size mbuf (Logger lref) = do
    logmsg <- atomicModifyIORef' lref (\old -> (mempty, old))
    -- If a special buffer is prepared for flusher, this MVar could
    -- be removed. But such a code does not contribute logging speed
    -- according to experiment. And even with the special buffer,
    -- there is no grantee that this function is exclusively called
    -- for a buffer. So, we use MVar here.
    -- This is safe and speed penalty can be ignored.
    withMVar mbuf $ \buf -> writeLogStr fdref buf size logmsg

----------------------------------------------------------------

-- | Writting 'LogStr' using a buffer in blocking mode.
--   The size of 'LogStr' must be smaller or equal to
--   the size of buffer.
writeLogStr :: IORef FD
            -> Buffer
            -> BufSize
            -> LogStr
            -> IO ()
writeLogStr fdref buf size (LogStr len builder)
  | size < len = error "writeLogStr"
  | otherwise  = toBufIOWith buf size (write fdref) builder

write :: IORef FD -> Buffer -> Int -> IO ()
write fdref buf len' = loop buf (fromIntegral len')
  where
    loop bf len = do
        written <- writeRawBufferPtr2FD fdref bf len
        when (0 <= written && written < len) $
            loop (bf `plusPtr` fromIntegral written) (len - written)
