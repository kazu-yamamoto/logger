{-# LANGUAGE BangPatterns #-}
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

import qualified Data.ByteString as BS
import Data.ByteString.Builder (Builder)
import qualified Data.ByteString.Builder as B
import qualified Data.ByteString.Char8 as S8
import qualified Data.ByteString.Lazy as BL
#if MIN_VERSION_base(4,9,0)
import qualified Data.Semigroup as Semi (Semigroup(..))
#endif
import Data.String (IsString(..))
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
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
--
-- In contrast to a normal 'Builder', it knows its length in Bytes.
--
-- It is guaranteed that when you evaluate a 'LogStr' to WHNF ('seq'),
-- the length will be evaluated; consequently,
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

-- | Types that can be converted to a 'LogStr'. Instances for
-- types from the @text@ library use a UTF-8 encoding. Instances
-- for numerical types use a decimal encoding.
--
-- As described in 'LogStr', a 'LogStr' knows its length.
-- That means that you cannot e.g. 'toLogStr' an infinite 'Builder'.
-- A 'Builder' that produces lots of output will not be logged in
-- a streaming fashion; it will be evaluated into memory!
class ToLogStr msg where
    -- | Converts the given argument into a 'LogStr'.
    toLogStr :: msg -> LogStr

instance ToLogStr LogStr where
    toLogStr = id
instance ToLogStr S8.ByteString where
    toLogStr bs = LogStr (BS.length bs) (toBuilder bs)
instance ToLogStr BL.ByteString where
    toLogStr b = LogStr (fromIntegral (BL.length b)) (B.lazyByteString b)
instance ToLogStr Builder where
    -- Bang to avoid floating it in and computing it twice; it's not necessary
    -- because we have `Strict` enabled for the project, but it's better to be
    -- explicit here given that this instance is used by most others.
    toLogStr x = let !b = B.toLazyByteString x in LogStr (fromIntegral (BL.length b)) (B.lazyByteString b)
instance ToLogStr String where
    toLogStr = toLogStr . TL.pack
instance ToLogStr T.Text where
    toLogStr = toLogStr . T.encodeUtf8
instance ToLogStr TL.Text where
    toLogStr = toLogStr . TL.encodeUtf8

-- | @since 2.4.14
instance ToLogStr Int where
    toLogStr = toLogStr . B.intDec
-- | @since 2.4.14
instance ToLogStr Int8 where
    toLogStr = toLogStr . B.int8Dec
-- | @since 2.4.14
instance ToLogStr Int16 where
    toLogStr = toLogStr . B.int16Dec
-- | @since 2.4.14
instance ToLogStr Int32 where
    toLogStr = toLogStr . B.int32Dec
-- | @since 2.4.14
instance ToLogStr Int64 where
    toLogStr = toLogStr . B.int64Dec

-- | @since 2.4.14
instance ToLogStr Word where
    toLogStr = toLogStr . B.wordDec
-- | @since 2.4.14
instance ToLogStr Word8 where
    toLogStr = toLogStr . B.word8Dec
-- | @since 2.4.14
instance ToLogStr Word16 where
    toLogStr = toLogStr . B.word16Dec
-- | @since 2.4.14
instance ToLogStr Word32 where
    toLogStr = toLogStr . B.word32Dec
-- | @since 2.4.14
instance ToLogStr Word64 where
    toLogStr = toLogStr . B.word64Dec

-- | @since 2.4.14
instance ToLogStr Integer where
    toLogStr = toLogStr . B.integerDec
-- | @since 2.4.14
instance ToLogStr Float where
    toLogStr = toLogStr . B.floatDec
-- | @since 2.4.14
instance ToLogStr Double where
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
