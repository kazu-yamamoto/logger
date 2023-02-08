{-# LANGUAGE RecordWildCards #-}

module System.Log.FastLogger.Write (
    BufFD(..)
  , writeLogStr
  , writeBigLogStr
  , Loggers(..)
  ) where

import Control.Concurrent (MVar, withMVar)
import Foreign.Marshal.Alloc (allocaBytes)
import Foreign.Ptr (plusPtr)

import System.Log.FastLogger.FileIO
import System.Log.FastLogger.IO
import System.Log.FastLogger.Imports
import System.Log.FastLogger.LogStr

----------------------------------------------------------------

data BufFD = BufFD {
    buffdFDRef    :: IORef FD
  , buffdBufSize  :: BufSize
  , buffdMVar     :: MVar Buffer
  }

-- | Writting 'LogStr' using a buffer in blocking mode.
--   The size of 'LogStr' must be smaller or equal to
--   the size of buffer.

writeLogStr :: BufFD -> LogStr -> IO ()
writeLogStr BufFD{..} (LogStr len builder)
  | buffdBufSize < len = error "writeLogStr"
  | otherwise  = withMVar buffdMVar $ \buf ->
      toBufIOWith buf len (write buffdFDRef) builder

writeBigLogStr :: BufFD -> LogStr -> IO ()
writeBigLogStr BufFD{..} (LogStr len builder) =  allocaBytes len $ \buf -> withMVar buffdMVar $ \_ ->
    toBufIOWith buf len (write buffdFDRef) builder

write :: IORef FD -> Buffer -> Int -> IO ()
write fdref buf len' = loop buf (fromIntegral len')
  where
    loop bf len = do
        written <- writeRawBufferPtr2FD fdref bf len
        when (0 <= written && written < len) $
            loop (bf `plusPtr` fromIntegral written) (len - written)

----------------------------------------------------------------

class Loggers a where
    stopLoggers :: a -> BufFD -> IO ()
    pushAllLog  :: a -> BufFD -> LogStr -> IO ()
    flushAllLog :: a -> BufFD -> IO ()
