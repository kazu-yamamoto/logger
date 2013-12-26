{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleInstances #-}

module System.Log.FastLogger.LogStr (
    Builder
  , LogStr(..)
  , logStrLength
  , fromLogStr
  , ToLogStr(..)
  , mempty
  , (<>)
  ) where

#if MIN_VERSION_bytestring(0,10,2)
import Data.ByteString.Builder (Builder)
import qualified Data.ByteString.Builder as B
#else
import qualified Blaze.ByteString.Builder as BB
import Blaze.ByteString.Builder.Internal.Types as BB (Builder(..))
#endif
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as S8
import Data.ByteString.Internal (ByteString(..))
import qualified Data.ByteString.Lazy as BL
import Data.Monoid (Monoid, mempty, mappend)
#if MIN_VERSION_base(4,5,0)
import Data.Monoid ((<>))
#endif
import Data.String (IsString(..))
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TL

----------------------------------------------------------------

#if !MIN_VERSION_base(4,5,0)
(<>) :: Monoid m => m -> m -> m
(<>) = mappend
#endif

toBuilder :: ByteString -> Builder
#if MIN_VERSION_bytestring(0,10,2)
toBuilder = B.byteString
#else
toBuilder = BB.fromByteString
#endif

fromBuilder :: Builder -> ByteString
#if MIN_VERSION_bytestring(0,10,2)
fromBuilder = BL.toStrict . B.toLazyByteString
#else
fromBuilder = BB.toByteString
#endif

----------------------------------------------------------------

-- | Log message builder. Use ('<>') to append two LogStr in O(1).
data LogStr = LogStr !Int Builder

instance Monoid LogStr where
    mempty = LogStr 0 (toBuilder BS.empty)
    LogStr s1 b1 `mappend` LogStr s2 b2 = LogStr (s1 + s2) (b1 <> b2)

instance IsString LogStr where
    fromString = toLogStr . TL.pack

class ToLogStr msg where
    toLogStr :: msg -> LogStr

instance ToLogStr LogStr where
    toLogStr = id
instance ToLogStr S8.ByteString where
    toLogStr bs = LogStr (BS.length bs) (toBuilder bs)
instance ToLogStr BL.ByteString where
    toLogStr = toLogStr . S8.concat . BL.toChunks
instance ToLogStr String where
    toLogStr = toLogStr . TL.pack
instance ToLogStr T.Text where
    toLogStr = toLogStr . T.encodeUtf8
instance ToLogStr TL.Text where
    toLogStr = toLogStr . TL.encodeUtf8

-- | Obtaining the length of 'LogStr'.
logStrLength :: LogStr -> Int
logStrLength (LogStr n _) = n

-- | Converting 'LogStr' to 'ByteString'.
fromLogStr :: LogStr -> ByteString
fromLogStr (LogStr _ builder) = fromBuilder builder
