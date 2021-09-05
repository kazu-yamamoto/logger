{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE Safe #-}

module System.Log.FastLogger.Logger (
    Logger(..)
  , newLogger
  , pushLog
  , flushLog
  , withFdAndBuffer_
  , modifyFd
  ) where


import Control.Concurrent (MVar, newMVar, withMVar, withMVarMasked, modifyMVarMasked)
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
      allocaBytes nlen $ \buf -> withFdAndBuffer_ fdmv mbuf $ \fd _buf ->
        writeLogStr fd buf nlen nlogmsg
  | otherwise = do
    mmsg <- atomicModifyIORef' ref checkBuf
    case mmsg of
        Nothing  -> return ()
        Just msg ->
            withFdAndBuffer_ fdmv mbuf $ \fd buf -> writeLogStr fd buf size msg
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
    withFdAndBuffer_ fdmv mbuf $ \fd buf -> writeLogStr fd buf size logmsg

----------------------------------------------------------------

-- | Writting 'LogStr' using a buffer in blocking mode.
--   The size of 'LogStr' must be smaller or equal to
--   the size of buffer.
writeLogStr :: FD
            -> Buffer
            -> BufSize
            -> LogStr
            -> IO ()
writeLogStr fd buf size (LogStr len builder)
  | size < len = error $ mconcat
    [ "writeLogStr: Message length is longer than expected size (buf size: "
    , show size
    , ", msg len: "
    , show len
    , ")"
    ]
  | otherwise = toBufIOWith buf size (write fd) builder

write :: FD -> Buffer -> Int -> IO ()
write fd buf len' = loop buf (fromIntegral len')
  where
    loop bf !len = do
        written <- writeRawBufferPtr2FD fd bf len
        when (0 <= written && written < len) $
            loop (bf `plusPtr` fromIntegral written) (len - written)


-- | Helper function to decide the order 'MVar's to prevent deadlock:
--   Take 'MVar FD' first, then take 'MVar Buffer'.
withFdAndBuffer_ :: MVar FD -> MVar Buffer -> (FD -> Buffer -> IO ()) -> IO ()
withFdAndBuffer_ fdmv bufmv fn =
    withMVarMasked fdmv $ \fd ->
    withMVar bufmv $ \buf -> fn fd buf

-- | 'modifyFd' is the same as 'modifyMVarMasked' but it tells developers that it's
--   safe way to get FD without deadlock.
modifyFd :: MVar FD -> (FD -> IO (FD, b)) -> IO b
modifyFd fdmv fn = modifyMVarMasked fdmv fn

