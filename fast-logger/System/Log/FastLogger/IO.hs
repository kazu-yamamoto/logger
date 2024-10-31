{-# LANGUAGE CPP #-}

#if __GLASGOW_HASKELL__ <= 708
{-# LANGUAGE Trustworthy #-}
#else
{-# LANGUAGE Safe #-}
#endif

module System.Log.FastLogger.IO where

import Data.ByteString.Builder.Extra (Next (..))
import qualified Data.ByteString.Builder.Extra as BBE
import Foreign.ForeignPtr (withForeignPtr)
import Foreign.Marshal.Alloc (free, mallocBytes)
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
toBufIOWith buf size io builder = loop $ BBE.runBuilder builder
  where
    loop writer = do
        (len, next) <- writer buf size
        io buf len
        case next of
            Done -> return ()
            More minSize writer'
                | size < minSize -> error "toBufIOWith: More: minSize"
                | otherwise -> loop writer'
            Chunk (PS fptr off siz) writer' ->
                withForeignPtr fptr $ \ptr -> io (ptr `plusPtr` off) siz >> loop writer'
