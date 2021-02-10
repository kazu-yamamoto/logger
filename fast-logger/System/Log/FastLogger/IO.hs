{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE CPP #-}

#if __GLASGOW_HASKELL__ <= 708
{-# LANGUAGE Trustworthy #-}
#else
{-# LANGUAGE Safe #-}
#endif

module System.Log.FastLogger.IO where

import Data.ByteString.Builder.Extra (Next(..))
import qualified Data.ByteString.Builder.Extra as BBE
import Foreign.ForeignPtr (ForeignPtr, withForeignPtr)
import Foreign.Marshal.Alloc (mallocBytes, free)
import Foreign.Ptr (Ptr, plusPtr)

import System.Log.FastLogger.Imports
import System.Log.FastLogger.LogStr

type Buffer = Ptr Word8

-- | The type for buffer size of each core.
type BufSize = Int

-- | The default buffer size (4,096 bytes).
defaultBufSize :: BufSize
defaultBufSize = 4096

getBuffer :: BufSize -> IO Buffer
getBuffer = mallocBytes

freeBuffer :: Buffer -> IO ()
freeBuffer = free

toBufIOWith :: Buffer -> BufSize -> (Buffer -> Int -> IO ()) -> Builder -> IO ()
toBufIOWith buf !size io builder = loop $ BBE.runBuilder builder
  where
    loop writer = do
        (len, next) <- writer buf size
        io buf len
        case next of
             Done -> return ()
             More minSize writer'
               | size < minSize -> error "toBufIOWith: More: minSize"
               | otherwise      -> loop writer'
             Chunk bs writer' -> flip withBS bs $ \fptr off siz ->
               withForeignPtr fptr $ \ptr -> io (ptr `plusPtr` off) siz >> loop writer'

----------------------------------------------------------------
-- Compatibility helpers for bytestring

withBS :: (ForeignPtr Word8 -> Int -> Int -> a) -> ByteString -> a
#if MIN_VERSION_bytestring(0,11,0)
withBS f (BS fptr len) = f fptr 0 len
#else
withBS f (PS fptr off len) = f fptr off len
#endif
