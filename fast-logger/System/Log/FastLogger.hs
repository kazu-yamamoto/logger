{-# LANGUAGE NoImplicitPrelude, RecordWildCards #-}

-- | Fast logging system to copy log data directly to Handle buffer.

module System.Log.FastLogger (
  -- * Initialization
    initHandle
  -- * Logging
  , LogStr(..)
  , hPutLogStr
  -- * Builder
  , hPutBuilder
  -- * File rotation
  , module System.Log.FastLogger.File
  ) where

import Blaze.ByteString.Builder
import qualified Data.ByteString as BS
import Data.ByteString.Internal (ByteString(..), c2w)
import Data.List
import Data.Maybe
import Data.Typeable
import Foreign
import GHC.Base
import GHC.IO.Buffer
import qualified GHC.IO.BufferedIO as Buffered
import qualified GHC.IO.Device as RawIO
import GHC.IO.FD
import GHC.IO.Handle.Internals
import GHC.IO.Handle.Text
import GHC.IO.Handle.Types
import GHC.IORef
import GHC.Num
import GHC.Real
import System.IO
import System.Log.FastLogger.File

{-| Setting a proper buffering to 'Handle'.
-}
initHandle :: Handle -> IO ()
initHandle hdl = hSetBuffering hdl (BlockBuffering (Just 4096))

{-| A date type to contain 'String' and 'ByteString'.
    This data is exported so that format can be defined.
    This would be replaced with 'Builder' someday when
    it can be written directly to 'Handle' buffer.
-}
data LogStr = LS !String | LB !ByteString

{-| The 'hPut' function to copy a list of 'LogStr' to the buffer
    of 'Handle' directly.
    If 'Handle' is associated with a file, 'AppendMode' must be used.
-}
hPutLogStr :: Handle -> [LogStr] -> IO ()
hPutLogStr handle bss =
  wantWritableHandle "hPutLogStr" handle $ \h_ -> bufsWrite h_ bss

-- based on GHC.IO.Handle.Text

bufsWrite :: Handle__ -> [LogStr] -> IO ()
bufsWrite h_@Handle__{..} bss = do
    old_buf@Buffer{
        bufRaw = old_raw
      , bufR = w
      , bufSize = size
      } <- readIORef haByteBuffer
    if size - w > len then do
        withRawBuffer old_raw $ \ptr ->
            go (ptr `plusPtr` w)  bss
        writeIORef haByteBuffer old_buf{ bufR = w + len }
     else do
        old_buf' <- Buffered.flushWriteBuffer haDevice old_buf
        writeIORef haByteBuffer old_buf'
        if size > len then
            bufsWrite h_ bss
         else allocaBytes size $ \ptr -> do
            go ptr bss
            let Just fd = cast haDevice :: Maybe FD
            RawIO.writeNonBlocking fd ptr size
            return ()
  where
    len = foldl' (\x y -> x + getLength y) 0 bss
    getLength (LB s) = BS.length s
    getLength (LS s) = length s
    go :: Ptr Word8 -> [LogStr] -> IO ()
    go _ [] = return ()
    go dst (LB b:bs) = do
      dst' <- copy dst b
      go dst' bs
    go dst (LS s:ss) = do
      dst' <- copy' dst s
      go dst' ss

copy :: Ptr Word8 -> ByteString -> IO (Ptr Word8)
copy dst (PS ptr off len) = withForeignPtr ptr $ \s -> do
    let src = s `plusPtr` off
    memcpy dst src (fromIntegral len)
    return (dst `plusPtr` len)

copy' :: Ptr Word8 -> String -> IO (Ptr Word8)
copy' dst [] = return dst
copy' dst (x:xs) = do
    poke dst (c2w x)
    copy' (dst `plusPtr` 1) xs

{-| The 'hPut' function directory to copy 'Builder' to the buffer.
    If 'Handle' is associated with a file, 'AppendMode' must be used.
    The current implementation is inefficient at this moment.
    'initHandle' must be called once beforehand if this function is used.
    This would replace 'hPutLogStr' someday.
-}
hPutBuilder :: Handle -> Builder -> IO ()
hPutBuilder hdl = BS.hPut hdl . toByteString
