{-# LANGUAGE BangPatterns, CPP #-}

module System.Log.FastLogger.IO where

#if MIN_VERSION_bytestring(0,10,2)
import Data.ByteString.Builder.Extra (Next(..))
import qualified Data.ByteString.Builder.Extra as BBE
#else
import Blaze.ByteString.Builder.Internal.Types (Builder(..), BuildSignal(..), BufRange(..), runBuildStep, buildStep)
import Foreign.Ptr (minusPtr)
#endif
import Data.ByteString.Internal (ByteString(..))
import Data.Word (Word8)
import Foreign.ForeignPtr (withForeignPtr)
import Foreign.Ptr (Ptr, plusPtr)
import System.Log.FastLogger.LogStr

type Buffer = Ptr Word8

-- | The type for buffer size of each core.
type BufSize = Int

-- | The default buffer size (4,096 bytes).
defaultBufSize :: BufSize
defaultBufSize = 4096

#if MIN_VERSION_bytestring(0,10,2)
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
             Chunk (PS fptr off siz) writer'
               | len == 0  -> loop writer' -- flushing
               | otherwise -> withForeignPtr fptr $ \ptr -> io (ptr `plusPtr` off) siz
#else
toBufIOWith :: Buffer -> BufSize -> (Buffer -> Int -> IO ()) -> Builder -> IO ()
toBufIOWith buf !size io (Builder build) = loop firstStep
  where
    firstStep = build (buildStep finalStep)
    finalStep (BufRange p _) = return $ Done p ()
    bufRange = BufRange buf (buf `plusPtr` size)
    loop step = do
        signal <- runBuildStep step bufRange
        case signal of
             Done ptr _ -> io buf (ptr `minusPtr` buf)
             BufferFull minSize ptr next
               | size < minSize -> error "toBufIOWith: BufferFull: minSize"
               | otherwise      -> do
                   io buf (ptr `minusPtr` buf)
                   loop next
             InsertByteString ptr (PS fptr off siz) next -> do
                 io buf (ptr `minusPtr` buf)
                 withForeignPtr fptr $ \p -> io (p `plusPtr` off) siz
                 loop next
#endif

