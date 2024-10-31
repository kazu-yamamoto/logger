{-# LANGUAGE RecordWildCards #-}

module System.Log.FastLogger.Write (
    writeLogStr,
    writeBigLogStr,
    Loggers (..),
) where

import Foreign.Marshal.Alloc (allocaBytes)
import Foreign.Ptr (plusPtr)

import System.Log.FastLogger.FileIO
import System.Log.FastLogger.IO
import System.Log.FastLogger.Imports
import System.Log.FastLogger.LogStr

----------------------------------------------------------------

-- | Writting 'LogStr' using a buffer in blocking mode.
--   The size of 'LogStr' must be smaller or equal to
--   the size of buffer.
writeLogStr :: Buffer -> IORef FD -> LogStr -> IO ()
writeLogStr buf fdref (LogStr len builder) =
    toBufIOWith buf len (write fdref) builder

-- | Writting 'LogStr' using a temporary buffer.
writeBigLogStr :: IORef FD -> LogStr -> IO ()
writeBigLogStr fdref (LogStr len builder) = allocaBytes len $ \buf ->
    toBufIOWith buf len (write fdref) builder

write :: IORef FD -> Buffer -> Int -> IO ()
write fdref buf len' = loop buf (fromIntegral len')
  where
    loop bf len = do
        written <- writeRawBufferPtr2FD fdref bf len
        when (0 <= written && written < len) $
            loop (bf `plusPtr` fromIntegral written) (len - written)

----------------------------------------------------------------

-- | A class for internal loggers.
class Loggers a where
    stopLoggers :: a -> IO ()
    pushLog :: a -> LogStr -> IO ()
    flushAllLog :: a -> IO ()
