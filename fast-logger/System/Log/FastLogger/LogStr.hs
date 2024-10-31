{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE Trustworthy #-}

module System.Log.FastLogger.LogStr (
    Builder,
    LogStr (..),
    logStrLength,
    fromLogStr,
    ToLogStr (..),
    mempty,
    (<>),
) where

import qualified Data.ByteString as BS
import Data.ByteString.Builder (Builder)
import qualified Data.ByteString.Builder as B
import qualified Data.ByteString.Char8 as S8
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Short as SBS
#if MIN_VERSION_base(4,9,0)
import qualified Data.Semigroup as Semi (Semigroup(..))
#endif
import Data.String (IsString (..))
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
#if MIN_VERSION_text(2,0,0)
import qualified Data.Text.Foreign as T
#endif
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TL

import System.Log.FastLogger.Imports

----------------------------------------------------------------

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
    {-# INLINE (<>) #-}
    LogStr s1 b1 <> LogStr s2 b2 = LogStr (s1 + s2) (b1 <> b2)
instance Monoid LogStr where
    mempty = LogStr 0 (toBuilder BS.empty)
#else
instance Monoid LogStr where
    mempty = LogStr 0 (toBuilder BS.empty)
    {-# INLINE mappend #-}
    LogStr s1 b1 `mappend` LogStr s2 b2 = LogStr (s1 + s2) (b1 <> b2)
#endif

instance IsString LogStr where
    {-# INLINE fromString #-}
    fromString = toLogStr . TL.pack

-- | Types that can be converted to a 'LogStr'. Instances for
-- types from the @text@ library use a UTF-8 encoding. Instances
-- for numerical types use a decimal encoding.
class ToLogStr msg where
    toLogStr :: msg -> LogStr

instance ToLogStr LogStr where
    {-# INLINE toLogStr #-}
    toLogStr = id
instance ToLogStr S8.ByteString where
    {-# INLINE toLogStr #-}
    toLogStr bs = LogStr (BS.length bs) (toBuilder bs)
instance ToLogStr BL.ByteString where
    {-# INLINE toLogStr #-}
    toLogStr b = LogStr (fromIntegral (BL.length b)) (B.lazyByteString b)
instance ToLogStr Builder where
    {-# INLINE toLogStr #-}
    toLogStr x =
        let b = B.toLazyByteString x
         in LogStr (fromIntegral (BL.length b)) (B.lazyByteString b)
instance ToLogStr SBS.ShortByteString where
    {-# INLINE toLogStr #-}
    toLogStr b = LogStr (SBS.length b) (B.shortByteString b)
instance ToLogStr String where
    {-# INLINE toLogStr #-}
    toLogStr = toLogStr . TL.pack
instance ToLogStr T.Text where
    {-# INLINE toLogStr #-}
#if MIN_VERSION_text(2,0,0)
    toLogStr t = LogStr (T.lengthWord8 t) (T.encodeUtf8Builder t)
#else
    toLogStr = toLogStr . T.encodeUtf8
#endif
instance ToLogStr TL.Text where
    {-# INLINE toLogStr #-}
#if MIN_VERSION_text(2,0,0)
    toLogStr t = LogStr (TL.foldlChunks (\n c -> T.lengthWord8 c + n) 0 t) (TL.encodeUtf8Builder t)
#else
    toLogStr = toLogStr . TL.encodeUtf8
#endif
-- | @since 2.4.14
instance ToLogStr Int where
    {-# INLINE toLogStr #-}
    toLogStr = toLogStr . B.intDec

-- | @since 2.4.14
instance ToLogStr Int8 where
    {-# INLINE toLogStr #-}
    toLogStr = toLogStr . B.int8Dec

-- | @since 2.4.14
instance ToLogStr Int16 where
    {-# INLINE toLogStr #-}
    toLogStr = toLogStr . B.int16Dec

-- | @since 2.4.14
instance ToLogStr Int32 where
    {-# INLINE toLogStr #-}
    toLogStr = toLogStr . B.int32Dec

-- | @since 2.4.14
instance ToLogStr Int64 where
    {-# INLINE toLogStr #-}
    toLogStr = toLogStr . B.int64Dec

-- | @since 2.4.14
instance ToLogStr Word where
    {-# INLINE toLogStr #-}
    toLogStr = toLogStr . B.wordDec

-- | @since 2.4.14
instance ToLogStr Word8 where
    {-# INLINE toLogStr #-}
    toLogStr = toLogStr . B.word8Dec

-- | @since 2.4.14
instance ToLogStr Word16 where
    {-# INLINE toLogStr #-}
    toLogStr = toLogStr . B.word16Dec

-- | @since 2.4.14
instance ToLogStr Word32 where
    {-# INLINE toLogStr #-}
    toLogStr = toLogStr . B.word32Dec

-- | @since 2.4.14
instance ToLogStr Word64 where
    {-# INLINE toLogStr #-}
    toLogStr = toLogStr . B.word64Dec

-- | @since 2.4.14
instance ToLogStr Integer where
    {-# INLINE toLogStr #-}
    toLogStr = toLogStr . B.integerDec

-- | @since 2.4.14
instance ToLogStr Float where
    {-# INLINE toLogStr #-}
    toLogStr = toLogStr . B.floatDec

-- | @since 2.4.14
instance ToLogStr Double where
    {-# INLINE toLogStr #-}
    toLogStr = toLogStr . B.doubleDec

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
