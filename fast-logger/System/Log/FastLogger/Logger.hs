{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE Safe #-}

module System.Log.FastLogger.Logger (
    Logger(..)
  , newLogger
  , pushLog
  , flushLog
  ) where


import Control.Concurrent (MVar, newMVar, withMVar)
import Foreign.Marshal.Alloc (allocaBytes)
import Foreign.Ptr (plusPtr)

import System.Log.FastLogger.FileIO
import System.Log.FastLogger.IO
import System.Log.FastLogger.Imports
import System.Log.FastLogger.LogStr

----------------------------------------------------------------

data Logger = Logger !BufSize (MVar Buffer) (IORef LogStr)

----------------------------------------------------------------

newLogger :: BufSize -> IO Logger
newLogger size = Logger size <$> (getBuffer size >>= newMVar)
                             <*> newIORef mempty

----------------------------------------------------------------

-- | In the below, it is important that `nlen` of the `LogStr` is evaluated
-- before taking the `mbuf` lock, to avoid holding the log unnecessarily long
-- to do a pure computation that could be done outside. See the docs of
-- `LoggingT` of `monad-logger` for an example.
pushLog :: IORef FD -> Logger -> LogStr -> IO ()
pushLog fdref logger@(Logger size mbuf ref) nlogmsg@(LogStr nlen nbuilder)
  | nlen > size = do
      flushLog fdref logger
      -- Make sure we have a large enough buffer to hold the entire
      -- contents, thereby allowing for a single write system call and
      -- avoiding interleaving. This does not address the possibility
      -- of write not writing the entire buffer at once.
      allocaBytes nlen $ \buf -> withMVar mbuf $ \_ ->
        toBufIOWith buf nlen (write fdref) nbuilder
  | otherwise = do
    mmsg <- atomicModifyIORef' ref checkBuf
    case mmsg of
        Nothing  -> return ()
        Just msg -> withMVar mbuf $ \buf -> writeLogStr fdref buf size msg
  where
    checkBuf ologmsg@(LogStr olen _)
      | size < olen + nlen = (nlogmsg, Just ologmsg)
      | otherwise          = (ologmsg <> nlogmsg, Nothing)

----------------------------------------------------------------

flushLog :: IORef FD -> Logger -> IO ()
flushLog fdref (Logger size mbuf lref) = do
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
    loop bf !len = do
        written <- writeRawBufferPtr2FD fdref bf len
        when (written < len) $
            loop (bf `plusPtr` fromIntegral written) (len - written)
