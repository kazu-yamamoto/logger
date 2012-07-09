{-# LANGUAGE NoImplicitPrelude, RecordWildCards #-}
{-# LANGUAGE FlexibleInstances #-}

-- | Fast logging system to copy log data directly to Handle buffer.

module System.Log.FastLogger (
  -- * Logger
    Logger
  , loggerAutoFlush
  , loggerHandle
  , loggerDateRef
  , mkLogger
  -- * Logging
  , loggerPutStr
  , loggerPutBuilder
  , loggerFlush
  -- * Strings
  , LogStr(..)
  , ToLogStr(..)
  -- * File rotation
  , module System.Log.FastLogger.File
  -- * Low level functions
  , initHandle
  ) where

import Blaze.ByteString.Builder
import Control.Applicative
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
import qualified Prelude
import System.IO
import System.Log.FastLogger.Date
import System.Log.FastLogger.File

import qualified Data.Text as TS
import qualified Data.Text.Encoding as TE
import qualified Data.Text.Lazy as TL
import qualified Data.ByteString as S
import qualified Data.ByteString.Lazy as L

-- | Abstract data type for logger.
-- To obtain inside data or to create a new one from another,
-- use selectors.
-- If 'Handle' is associated with a file, 'AppendMode' must be used.
data Logger = Logger {
    -- | Selector for autoFlush.
    loggerAutoFlush :: Bool
    -- | Selector for 'Handle'.
  , loggerHandle    :: Handle
    -- | Selector for 'DateRef'.
  , loggerDateRef   :: DateRef
  }

logBufSize :: Int
logBufSize = 4096

-- | Setting a proper buffering to 'Handle'.
-- You don't have to call this function before you call 'mkLogger'.
initHandle :: Handle -> IO ()
initHandle hdl = hSetBuffering hdl (BlockBuffering (Just logBufSize))

{-| Creates a 'Logger' from the given handle.
-}
mkLogger :: Bool -- ^ automatically flush on each write?
         -> Handle
         -> IO Logger
mkLogger autoFlush hdl = do
    initHandle hdl
    Logger autoFlush hdl <$> dateInit

{-| A date type to contain 'String' and 'ByteString'.
    This data is exported so that format can be defined.
    This would be replaced with 'Builder' someday when
    it can be written directly to 'Handle' buffer.
-}
data LogStr = LS !String | LB !ByteString

class ToLogStr a where toLogStr :: a -> LogStr
instance ToLogStr [Char] where toLogStr = LS
instance ToLogStr ByteString where toLogStr = LB
instance ToLogStr L.ByteString where toLogStr = LB . S.concat . L.toChunks
instance ToLogStr TS.Text where toLogStr = LB . TE.encodeUtf8
instance ToLogStr TL.Text where toLogStr = LB . TE.encodeUtf8 . TL.toStrict

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
            _ <- RawIO.writeNonBlocking fd ptr size
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
    _ <- memcpy dst src (fromIntegral len)
    return (dst `plusPtr` len)

copy' :: Ptr Word8 -> String -> IO (Ptr Word8)
copy' dst [] = return dst
copy' dst (x:xs) = do
    poke dst (c2w x)
    copy' (dst `plusPtr` 1) xs

-- | The 'hPut' function to copy a list of 'LogStr' to the buffer
-- of 'Handle' of 'Logger' directly.
loggerPutStr :: Logger -> [LogStr] -> IO ()
loggerPutStr logger strs
  | autoflush = hPutLogStr hdl strs >> hFlush hdl
  | otherwise = hPutLogStr hdl strs
  where
    autoflush = loggerAutoFlush logger
    hdl = loggerHandle logger

-- | The 'hPut' function directory to copy 'Builder' to the buffer.
-- The current implementation is inefficient at this moment.
-- This would replace 'loggerPutStr' someday.
loggerPutBuilder :: Logger -> Builder -> IO ()
loggerPutBuilder logger = loggerPutStr logger . return . LB . toByteString

-- | Flushing the buffer of 'Handle' of 'Logger'.
loggerFlush :: Logger -> IO ()
loggerFlush logger = hFlush $ loggerHandle logger
