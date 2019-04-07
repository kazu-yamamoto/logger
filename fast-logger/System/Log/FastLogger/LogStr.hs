{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE Safe #-}

module System.Log.FastLogger.LogStr (
    Builder
  , LogStr(..)
  , logStrLength
  , fromLogStr
  , ToLogStr(..)
  , mempty
  , (<>)
  ) where

import Data.ByteString.Builder (Builder)
import qualified Data.ByteString.Builder as B
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as S8
import qualified Data.ByteString.Lazy as BL
import Data.Int (Int8,Int16,Int32,Int64)
#if __GLASGOW_HASKELL__ < 709
import Data.Monoid (Monoid, mempty, mappend)
#endif
#if MIN_VERSION_base(4,5,0)
import Data.Monoid ((<>))
#endif
#if MIN_VERSION_base(4,9,0)
import qualified Data.Semigroup as Semi (Semigroup(..))
#endif
import Data.String (IsString(..))
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TL
import Data.Word (Word8,Word16,Word32,Word64)

----------------------------------------------------------------

#if !MIN_VERSION_base(4,5,0)
(<>) :: Monoid m => m -> m -> m
(<>) = mappend
#endif

toBuilder :: ByteString -> Builder
toBuilder = B.byteString

fromBuilder :: Builder -> ByteString
#if MIN_VERSION_bytestring(0,10,0)
fromBuilder = BL.toStrict . B.toLazyByteString
#else
fromBuilder = BS.concat . BL.toChunks . B.toLazyByteString
#endif

----------------------------------------------------------------

-- | Log message builder. Use ('<>') to append two LogStr in O(1).
data LogStr = LogStr !Int Builder

#if MIN_VERSION_base(4,9,0)
instance Semi.Semigroup LogStr where
    LogStr s1 b1 <> LogStr s2 b2 = LogStr (s1 + s2) (b1 <> b2)
#endif

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
    toLogStr b = LogStr (fromIntegral (BL.length b)) (B.lazyByteString b)
instance ToLogStr Builder where
    toLogStr x = let b = B.toLazyByteString x in LogStr (fromIntegral (BL.length b)) (B.lazyByteString b)
instance ToLogStr String where
    toLogStr = toLogStr . TL.pack
instance ToLogStr T.Text where
    toLogStr = toLogStr . T.encodeUtf8
instance ToLogStr TL.Text where
    toLogStr = toLogStr . TL.encodeUtf8
instance ToLogStr Int where
  toLogStr = toLogStr . B.intDec
instance ToLogStr Int8 where
  toLogStr = toLogStr . B.int8Dec
instance ToLogStr Int16 where
  toLogStr = toLogStr . B.int16Dec
instance ToLogStr Int32 where
  toLogStr = toLogStr . B.int32Dec
instance ToLogStr Int64 where
  toLogStr = toLogStr . B.int64Dec
instance ToLogStr Word where
  toLogStr = toLogStr . B.wordDec
instance ToLogStr Word8 where
  toLogStr = toLogStr . B.word8Dec
instance ToLogStr Word16 where
  toLogStr = toLogStr . B.word16Dec
instance ToLogStr Word32 where
  toLogStr = toLogStr . B.word32Dec
instance ToLogStr Word64 where
  toLogStr = toLogStr . B.word64Dec

instance Show LogStr where
  show = show . T.decodeUtf8 . fromLogStr

instance Eq LogStr where
  a == b = fromLogStr a == fromLogStr b

-- | Obtaining the length of 'LogStr'.
logStrLength :: LogStr -> Int
logStrLength (LogStr n _) = n

-- | Converting 'LogStr' to 'ByteString'.
fromLogStr :: LogStr -> ByteString
fromLogStr (LogStr _ builder) = fromBuilder builder
