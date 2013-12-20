{-# LANGUAGE BangPatterns, CPP #-}
{-# LANGUAGE FlexibleInstances #-}

module System.Log.FastLogger (
  -- * Creating a logger set
    LoggerSet
  , BufSize
  , defaultBufSize
  , logOpen
  , newLoggerSet
  , renewLoggerSet
  -- * Removing a logger set
  , rmLoggerSet
  -- * Log messages
  , LogStr
  , ToLogStr(..)
  , logStrLength
  , logStrBuilder
  -- * Writing a log message
  , pushLogStr
  -- * Flushing buffered log messages
  , flushLogStr
  -- * File rotation
  , module System.Log.FastLogger.File
  ) where

#if MIN_VERSION_bytestring(0,10,2)
import Data.ByteString.Builder (Builder, byteString)
import Data.ByteString.Builder.Extra (Next(..))
import qualified Data.ByteString.Builder.Extra as BBE
#else
import Blaze.ByteString.Builder.Internal.Types (Builder(..), BuildSignal(..), BufRange(..), runBuildStep, buildStep)
import qualified Blaze.ByteString.Builder as BB
import Foreign.Ptr (minusPtr)
#endif
import Control.Applicative ((<$>))
import Control.Concurrent (getNumCapabilities, myThreadId, threadCapability, MVar, newMVar, takeMVar, withMVar)
import Control.Monad (when, replicateM)
import Data.Array (Array, listArray, (!))
import Data.ByteString.Internal (ByteString(..))
import Data.IORef
import Data.Monoid (Monoid, mempty, mappend)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as S8
import qualified Data.ByteString.Lazy as L
#if MIN_VERSION_base(4,5,0)
import Data.Monoid ((<>))
#endif
import Data.String (IsString(..))
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TL
import Data.Word (Word8)
import Foreign.ForeignPtr (withForeignPtr)
import Foreign.Marshal.Alloc (mallocBytes, free)
import Foreign.Ptr (Ptr, plusPtr)
import GHC.IO.Device (close)
import GHC.IO.FD (FD(..), openFile, writeRawBufferPtr)
import GHC.IO.IOMode (IOMode(..))
import System.Log.FastLogger.File

----------------------------------------------------------------

#if !MIN_VERSION_base(4,5,0)
(<>) :: Monoid m => m -> m -> m
(<>) = mappend
#endif

#if !MIN_VERSION_bytestring(0,10,2)
byteString :: ByteString -> Builder
byteString = BB.fromByteString
#endif

----------------------------------------------------------------

type Buffer = Ptr Word8

-- | The type for buffer size of each core.
type BufSize = Int

-- | The default buffer size (4,096 bytes).
defaultBufSize :: BufSize
defaultBufSize = 4096

----------------------------------------------------------------

-- | Log message builder. Use ('<>') to append two LogStr in O(1).
data LogStr = LogStr {
  -- | Obtaining the length of 'LogStr'.
    logStrLength :: !Int
  -- | Obtaining the 'Builder' of 'LogStr'.
  , logStrBuilder :: Builder
  }

instance Monoid LogStr where
    mempty = LogStr 0 (byteString BS.empty)
    LogStr s1 b1 `mappend` LogStr s2 b2 = LogStr (s1 + s2) (b1 <> b2)

instance IsString LogStr where
    fromString = toLogStr . TL.pack

class ToLogStr msg where
    toLogStr :: msg -> LogStr

instance ToLogStr LogStr where
    toLogStr = id
instance ToLogStr S8.ByteString where
    toLogStr = fromByteString
instance ToLogStr L.ByteString where
    toLogStr = fromByteString . S8.concat . L.toChunks
instance ToLogStr String where
    toLogStr = toLogStr . TL.pack
instance ToLogStr T.Text where
    toLogStr = toLogStr . T.encodeUtf8
instance ToLogStr TL.Text where
    toLogStr = toLogStr . TL.encodeUtf8

-- | Creating 'LogStr'
fromByteString :: ByteString -> LogStr
fromByteString bs = LogStr (BS.length bs) (byteString bs)

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
  | size < len = error "writeLogStr"
  | otherwise  = toBufIOWith buf size (write fd) builder

-- FIXME:
-- | After this function call, Buffer should be empty
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
    finalStep !(BufRange p _) = return $ Done p ()
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

write :: FD -> Buffer -> Int -> IO ()
write fd buf len' = loop buf (fromIntegral len')
  where
    loop bf !len = do
        written <- writeRawBufferPtr "write" fd bf 0 (fromIntegral len)
        when (written < len) $
            loop (bf `plusPtr` fromIntegral written) (len - written)

----------------------------------------------------------------

data Logger = Logger (MVar Buffer) !BufSize (IORef LogStr)

newLogger :: BufSize -> IO Logger
newLogger size = do
    buf <- getBuffer size
    mbuf <- newMVar buf
    lref <- newIORef mempty
    return $ Logger mbuf size lref

pushLog :: FD -> Logger -> LogStr -> IO ()
pushLog fd logger@(Logger mbuf size ref) nlogmsg@(LogStr nlen nbuilder)
  | nlen > size = do
      flushLog fd logger
      -- FIXME: lock me!
      withMVar mbuf $ \buf -> toBufIOWith buf nlen (write fd) nbuilder
  | otherwise = do
    needFlush <- atomicModifyIORef' ref checkBuf
    when needFlush $ do
        flushLog fd logger
        pushLog fd logger nlogmsg
  where
    checkBuf ologmsg@(LogStr olen _)
      | size < olen + nlen = (ologmsg, True)
      | otherwise          = (ologmsg <> nlogmsg, False)

flushLog :: FD -> Logger -> IO ()
flushLog fd (Logger mbuf size lref) = do
    logmsg <- atomicModifyIORef' lref (\old -> (mempty, old))
    -- If a special buffer is prepared for flusher, this MVar could
    -- be removed. But such a code does not contribute logging speed
    -- according to experiment. And even with the special buffer,
    -- there is no grantee that this function is exclusively called
    -- for a buffer. So, we use MVar here.
    -- This is safe and speed penalty can be ignored.
    withMVar mbuf $ \buf -> writeLogStr fd buf size logmsg

----------------------------------------------------------------

-- | Opening a log file. FIXME: Windows support.
logOpen :: FilePath -> IO FD
logOpen file = fst <$> openFile file AppendMode False -- FIXME blocking

getBuffer :: BufSize -> IO Buffer
getBuffer = mallocBytes

----------------------------------------------------------------

-- | A set of loggers.
--   The number of loggers is the capabilities of GHC RTS.
--   You can specify it with \"+RTS -N\<x\>\".
--   A buffer is prepared for each capability.
data LoggerSet = LoggerSet (IORef FD) (Array Int Logger)

-- | Creating a new 'LoggerSet'.
newLoggerSet :: BufSize -> FD -> IO LoggerSet
newLoggerSet size fd = do
    n <- getNumCapabilities
    loggers <- replicateM n $ newLogger size
    let arr = listArray (0,n-1) loggers
    fref <- newIORef fd
    return $ LoggerSet fref arr

-- | Writing a log message to the corresponding buffer.
pushLogStr :: LoggerSet -> LogStr -> IO ()
pushLogStr (LoggerSet fref arr) logmsg = do
    (i, _) <- myThreadId >>= threadCapability
    let logger = arr ! i
    fd <- readIORef fref
    pushLog fd logger logmsg

-- | Flushing log messages in buffers.
flushLogStr :: LoggerSet -> IO ()
flushLogStr (LoggerSet fref arr) = do
    n <- getNumCapabilities
    fd <- readIORef fref
    mapM_ (flushIt fd) [0..n-1]
  where
    flushIt fd i = flushLog fd (arr ! i)

-- | Renewing 'FD' in 'LoggerSet'. Old 'FD' is closed.
renewLoggerSet :: LoggerSet -> FD -> IO ()
renewLoggerSet (LoggerSet fref _) newfd = do
    oldfd <- atomicModifyIORef' fref (\fd -> (newfd, fd))
    close oldfd

-- | Flushing the buffers, closing 'FD' and freeing the buffers.
rmLoggerSet :: LoggerSet -> IO ()
rmLoggerSet (LoggerSet fref arr) = do
    n <- getNumCapabilities
    fd <- readIORef fref
    let nums = [0..n-1]
    mapM_ (flushIt fd) nums
    mapM_ freeIt nums
    when (fdFD fd /= 1) $ close fd
  where
    flushIt fd i = flushLog fd (arr ! i)
    freeIt i = do
        let (Logger mbuf _ _) = arr ! i
        takeMVar mbuf >>= free

#if !MIN_VERSION_base(4,6,0)
-- | Strict version of 'atomicModifyIORef'.  This forces both the value stored
-- in the 'IORef' as well as the value returned.
atomicModifyIORef' :: IORef a -> (a -> (a,b)) -> IO b
atomicModifyIORef' ref f = do
    c <- atomicModifyIORef ref
            (\x -> let (a, b) = f x    -- Lazy application of "f"
                    in (a, a `seq` b)) -- Lazy application of "seq"
    -- The following forces "a `seq` b", so it also forces "f x".
    c `seq` return c
#endif
