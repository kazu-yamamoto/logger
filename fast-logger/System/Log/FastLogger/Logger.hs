{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE Safe #-}

module System.Log.FastLogger.Logger (
    Logger(..)
  , newLogger
  , pushLog
  , flushLog
  ) where


import Control.Concurrent (MVar, newMVar, withMVar, withMVarMasked)
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

pushLog :: MVar FD -> Logger -> LogStr -> IO ()
pushLog fdmv logger@(Logger size mbuf ref) nlogmsg@(LogStr nlen _nbuilder)
  | nlen > size = do
      flushLog fdmv logger
      -- Make sure we have a large enough buffer to hold the entire
      -- contents, thereby allowing for a single write system call and
      -- avoiding interleaving. This does not address the possibility
      -- of write not writing the entire buffer at once.
      allocaBytes nlen $ \buf -> withMVar mbuf $ \_ ->
        writeLogStr fdmv buf nlen nlogmsg
  | otherwise = do
    mmsg <- atomicModifyIORef' ref checkBuf
    case mmsg of
        Nothing  -> return ()
        Just msg -> withMVar mbuf $ \buf -> writeLogStr fdmv buf size msg
  where
    checkBuf ologmsg@(LogStr olen _)
      | size < olen + nlen = (nlogmsg, Just ologmsg)
      | otherwise          = (ologmsg <> nlogmsg, Nothing)

----------------------------------------------------------------

flushLog :: MVar FD -> Logger -> IO ()
flushLog fdmv (Logger size mbuf lref) = do
    logmsg <- atomicModifyIORef' lref (\old -> (mempty, old))
    -- If a special buffer is prepared for flusher, this MVar could
    -- be removed. But such a code does not contribute logging speed
    -- according to experiment. And even with the special buffer,
    -- there is no grantee that this function is exclusively called
    -- for a buffer. So, we use MVar here.
    -- This is safe and speed penalty can be ignored.
    withMVar mbuf $ \buf -> writeLogStr fdmv buf size logmsg

----------------------------------------------------------------

-- | Writting 'LogStr' using a buffer in blocking mode.
--   The size of 'LogStr' must be smaller or equal to
--   the size of buffer.
writeLogStr :: MVar FD
            -> Buffer
            -> BufSize
            -> LogStr
            -> IO ()
writeLogStr fdmv buf size (LogStr len builder)
  | size < len = error $ mconcat
    [ "writeLogStr: Message length is longer than expected size (buf size: "
    , show size
    , ", msg len: "
    , show len
    , ")"
    ]
  | otherwise  =
    -- NOTE: Guard fd for thread-safe even if async exceptions are thrown to this thread
    withMVarMasked fdmv $ \fd -> toBufIOWith buf size (write fd) builder

write :: FD -> Buffer -> Int -> IO ()
write fd buf len' = loop buf (fromIntegral len')
  where
    loop bf !len = do
        written <- writeRawBufferPtr2FD fd bf len
        when (0 <= written && written < len) $
            loop (bf `plusPtr` fromIntegral written) (len - written)
