{-# LANGUAGE NoImplicitPrelude, RecordWildCards #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TemplateHaskell #-}

-- | Fast logging system to copy log data directly to Handle buffer.

module System.Log.FastLogger (
  -- * Initialization
    mkLogger
  -- * Logging
  , Logger
  , loggerPutStr
  , loggerPutBuilder
  , loggerDateRef
  -- * Strings
  , LogStr(..)
  , ToLogStr(..)
  -- * MonadLogging
  , MonadLogging(..)
  , LogLevel(..)
  -- * TH logging
  , logDebug
  , logInfo
  , logWarn
  , logError
  , logOther
  -- * File rotation
  , module System.Log.FastLogger.File
  ) where

import qualified Prelude
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
import System.Log.FastLogger.Date

import Language.Haskell.TH.Syntax (Loc (Loc), Lift (lift), Q, Exp, qLocation)
import Data.Text (Text)

import qualified Data.Text as TS
import qualified Data.Text.Encoding as TE
import qualified Data.Text.Lazy as TL
import qualified Data.ByteString as S
import qualified Data.ByteString.Lazy as L

import Data.Monoid (Monoid)

import Data.Functor.Identity (Identity)
import Control.Monad.ST (ST)
import qualified Control.Monad.ST.Lazy as Lazy (ST)

import Control.Monad.Trans.Identity ( IdentityT)
import Control.Monad.Trans.List     ( ListT    )
import Control.Monad.Trans.Maybe    ( MaybeT   )
import Control.Monad.Trans.Error    ( ErrorT, Error)
import Control.Monad.Trans.Reader   ( ReaderT  )
import Control.Monad.Trans.Cont     ( ContT  )
import Control.Monad.Trans.State    ( StateT   )
import Control.Monad.Trans.Writer   ( WriterT  )
import Control.Monad.Trans.RWS      ( RWST     )
import Control.Monad.Trans.Resource ( ResourceT)

import qualified Control.Monad.Trans.RWS.Strict    as Strict ( RWST   )
import qualified Control.Monad.Trans.State.Strict  as Strict ( StateT )
import qualified Control.Monad.Trans.Writer.Strict as Strict ( WriterT )

import Control.Monad.Trans.Class (MonadTrans)
import qualified Control.Monad.Trans.Class as Trans

data Logger = Logger
    { loggerPutStr :: [LogStr] -> IO ()
    , loggerDateRef :: DateRef
    }

{-| Creates a @Logger@ from the given handle.
-}
mkLogger :: Bool -- ^ automatically flush on each write?
         -> Handle
         -> IO Logger
mkLogger autoFlush hdl = do
  hSetBuffering hdl (BlockBuffering (Just 4096))
  dateref <- dateInit
  let put strs
        | autoFlush = hPutLogStr hdl strs >> hFlush hdl
        | otherwise = hPutLogStr hdl strs
  return (Logger put dateref)


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
loggerPutBuilder :: Logger -> Builder -> IO ()
loggerPutBuilder logger = loggerPutStr logger . return . LB . toByteString

data LogLevel = LevelDebug | LevelInfo | LevelWarn | LevelError | LevelOther TS.Text
    deriving (Eq, Prelude.Show, Prelude.Read, Ord)

instance Lift LogLevel where
    lift LevelDebug = [|LevelDebug|]
    lift LevelInfo = [|LevelInfo|]
    lift LevelWarn = [|LevelWarn|]
    lift LevelError = [|LevelError|]
    lift (LevelOther x) = [|LevelOther $ TS.pack $(lift $ TS.unpack x)|]

class Monad m => MonadLogging m where
    monadLoggingLog :: ToLogStr msg => Loc -> LogLevel -> msg -> m ()

instance MonadLogging IO          where monadLoggingLog _ _ _ = return ()
instance MonadLogging Identity    where monadLoggingLog _ _ _ = return ()
instance MonadLogging (ST s)      where monadLoggingLog _ _ _ = return ()
instance MonadLogging (Lazy.ST s) where monadLoggingLog _ _ _ = return ()

liftLog :: (MonadTrans t, MonadLogging m, ToLogStr msg) => Loc -> LogLevel -> msg -> t m ()
liftLog a b c = Trans.lift $ monadLoggingLog a b c

instance MonadLogging m => MonadLogging (IdentityT m) where monadLoggingLog = liftLog
instance MonadLogging m => MonadLogging (ListT m) where monadLoggingLog = liftLog
instance MonadLogging m => MonadLogging (MaybeT m) where monadLoggingLog = liftLog
instance (MonadLogging m, Error e) => MonadLogging (ErrorT e m) where monadLoggingLog = liftLog
instance MonadLogging m => MonadLogging (ReaderT r m) where monadLoggingLog = liftLog
instance MonadLogging m => MonadLogging (ContT r m) where monadLoggingLog = liftLog
instance MonadLogging m => MonadLogging (StateT s m) where monadLoggingLog = liftLog
instance (MonadLogging m, Monoid w) => MonadLogging (WriterT w m) where monadLoggingLog = liftLog
instance (MonadLogging m, Monoid w) => MonadLogging (RWST r w s m) where monadLoggingLog = liftLog
instance MonadLogging m => MonadLogging (ResourceT m) where monadLoggingLog = liftLog
instance MonadLogging m => MonadLogging (Strict.StateT s m) where monadLoggingLog = liftLog
instance (MonadLogging m, Monoid w) => MonadLogging (Strict.WriterT w m) where monadLoggingLog = liftLog
instance (MonadLogging m, Monoid w) => MonadLogging (Strict.RWST r w s m) where monadLoggingLog = liftLog

logTH :: LogLevel -> Q Exp
logTH level =
    [|monadLoggingLog $(qLocation >>= liftLoc) $(lift level)|]
  where
    liftLoc :: Loc -> Q Exp
    liftLoc (Loc a b c d e) = [|Loc $(lift a) $(lift b) $(lift c) $(lift d) $(lift e)|]

-- | Generates a function that takes a 'Text' and logs a 'LevelDebug' message. Usage:
--
-- > $(logDebug) "This is a debug log message"
logDebug :: Q Exp
logDebug = logTH LevelDebug

-- | See 'logDebug'
logInfo :: Q Exp
logInfo = logTH LevelInfo
-- | See 'logDebug'
logWarn :: Q Exp
logWarn = logTH LevelWarn
-- | See 'logDebug'
logError :: Q Exp
logError = logTH LevelError

-- | Generates a function that takes a 'Text' and logs a 'LevelOther' message. Usage:
--
-- > $(logOther "My new level") "This is a log message"
logOther :: Text -> Q Exp
logOther = logTH . LevelOther
