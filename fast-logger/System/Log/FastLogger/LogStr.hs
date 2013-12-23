{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleInstances #-}

module System.Log.FastLogger.LogStr (
    Builder
  , LogStr(..)
  , ToLogStr(..)
  , mempty
  , (<>)
  ) where

#if MIN_VERSION_bytestring(0,10,2)
import Data.ByteString.Builder (Builder, byteString)
#else
import qualified Blaze.ByteString.Builder as BB (fromByteString)
import Blaze.ByteString.Builder.Internal.Types as BB (Builder(..))
#endif
import Data.ByteString.Internal (ByteString(..))
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
