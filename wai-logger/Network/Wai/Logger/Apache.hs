{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

module Network.Wai.Logger.Apache (
    IPAddrSource (..),
    apacheLogStr,
    serverpushLogStr,
) where

#ifndef MIN_VERSION_base
#define MIN_VERSION_base(x,y,z) 1
#endif
#ifndef MIN_VERSION_wai
#define MIN_VERSION_wai(x,y,z) 1
#endif

import qualified Data.ByteString as B
import Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Internal as BI
import qualified Data.ByteString.Unsafe as BU
import Data.Char (isSpace)
import Data.List (find)
import Data.Maybe (fromMaybe)
import Data.Word (Word8)
import Foreign.Ptr (Ptr)
import Foreign.Storable (pokeByteOff)
#if MIN_VERSION_base(4,5,0)
import Data.Monoid ((<>), First (..))
#else
import Data.Monoid (mappend)
#endif
import Network.HTTP.Types (Status, statusCode)
import Network.HTTP.Types.Header (HeaderName)
import Network.Wai (Request (..))
import Network.Wai.Logger.IP
import System.Log.FastLogger

-- $setup
-- >>> :set -XOverloadedStrings
-- >>> import Network.Wai (defaultRequest)

-- | Source from which the IP source address of the client is obtained.
data IPAddrSource
    = -- | From the peer address of the HTTP connection.
      FromSocket
    | -- | From @X-Real-IP@ or @X-Forwarded-For@ in the HTTP header.
      --
      -- This picks either @X-Real-IP@ or @X-Forwarded-For@ depending on which of these
      -- headers comes first in the ordered list of request headers.
      --
      -- If the @X-Forwarded-For@ header is picked, the value will be assumed to be a
      -- comma-separated list of IP addresses.  The value will be parsed, and the
      -- left-most IP address will be used (which is mostly likely to be the actual
      -- client IP address).
      FromHeader
    | -- | From a custom HTTP header, useful in proxied environment.
      --
      -- The header value will be assumed to be a comma-separated list of IP
      -- addresses.  The value will be parsed, and the left-most IP address will be
      -- used (which is mostly likely to be the actual client IP address).
      --
      -- Note that this still works as expected for a single IP address.
      FromHeaderCustom [HeaderName]
    | -- | Just like 'FromHeader', but falls back on the peer address if header is not found.
      FromFallback
    | -- | This gives you the most flexibility to figure out the IP source address
      -- from the 'Request'.  The returned 'ByteString' is used as the IP source
      -- address.
      FromRequest (Request -> ByteString)

-- | Apache style log format.
apacheLogStr
    :: ToLogStr user
    => IPAddrSource
    -> (Request -> Maybe user)
    -> FormattedTime
    -> Request
    -> Status
    -> Maybe Integer
    -> LogStr
apacheLogStr ipsrc userget tmstr req status msize =
    toLogStr (escapeLogItem (getSourceIP ipsrc req))
        <> " - "
        <> maybe "-" escapeUser (userget req)
        <> " ["
        <> toLogStr tmstr
        <> "] \""
        <> toLogStr (escapeLogItem (requestMethod req))
        <> " "
        <> toLogStr (escapeLogItem path)
        <> " "
        <> toLogStr (show (httpVersion req))
        <> "\" "
        <> toLogStr (show (statusCode status))
        <> " "
        <> toLogStr (maybe "-" show msize)
        <> " \""
        <> toLogStr (escapeLogItem (fromMaybe "" mr))
        <> "\" \""
        <> toLogStr (escapeLogItem (fromMaybe "" mua))
        <> "\"\n"
  where
    path = rawPathInfo req <> rawQueryString req
#if !MIN_VERSION_base(4,5,0)
    (<>) = mappend
#endif
#if MIN_VERSION_wai(3,2,0)
    mr  = requestHeaderReferer req
    mua = requestHeaderUserAgent req
#else
    mr  = lookup "referer" $ requestHeaders req
    mua = lookup "user-agent" $ requestHeaders req
#endif

-- | HTTP/2 Push log format in the Apache style.
serverpushLogStr
    :: ToLogStr user
    => IPAddrSource
    -> (Request -> Maybe user)
    -> FormattedTime
    -> Request
    -> ByteString
    -> Integer
    -> LogStr
serverpushLogStr ipsrc userget tmstr req path size =
    toLogStr (escapeLogItem (getSourceIP ipsrc req))
        <> " - "
        <> maybe "-" escapeUser (userget req)
        <> " ["
        <> toLogStr tmstr
        <> "] \"PUSH "
        <> toLogStr (escapeLogItem path)
        <> " HTTP/2\" 200 "
        <> toLogStr (show size)
        <> " \""
        <> toLogStr (escapeLogItem ref)
        <> "\" \""
        <> toLogStr (escapeLogItem (fromMaybe "" mua))
        <> "\"\n"
  where
    ref = rawPathInfo req
#if !MIN_VERSION_base(4,5,0)
    (<>) = mappend
#endif
#if MIN_VERSION_wai(3,2,0)
    mua = requestHeaderUserAgent req
#else
    mua = lookup "user-agent" $ requestHeaders req
#endif

escapeUser :: ToLogStr user => user -> LogStr
escapeUser = toLogStr . escapeLogItem . fromLogStr . toLogStr

-- | Escaping a request-derived field as Apache's @ap_escape_logitem@ does.
--   No copy is made if nothing needs to be escaped.
--
-- >>> escapeLogItem "Mozilla/5.0"
-- "Mozilla/5.0"
-- >>> escapeLogItem "a\"b\\c\r\n\x01\x7f\xe3"
-- "a\\\"b\\\\c\\r\\n\\x01\\x7f\\xe3"
escapeLogItem :: ByteString -> ByteString
escapeLogItem bs
    | extra == 0 = bs
    | otherwise = BI.unsafeCreate (len + extra) $ \p -> fill p 0 0
  where
    len = B.length bs
    extra = B.foldl' (\n w -> n + escapeLen w) 0 bs
    fill :: Ptr Word8 -> Int -> Int -> IO ()
    fill p i j
        | i >= len = return ()
        | otherwise = do
            let w = BU.unsafeIndex bs i
            case escapeLen w of
                0 -> do
                    pokeByteOff p j w
                    fill p (i + 1) (j + 1)
                1 -> do
                    pokeByteOff p j backslash
                    pokeByteOff p (j + 1) (escapeChar w)
                    fill p (i + 1) (j + 2)
                _ -> do
                    pokeByteOff p j backslash
                    pokeByteOff p (j + 1) (0x78 :: Word8) -- 'x'
                    pokeByteOff p (j + 2) (hex (w `div` 16))
                    pokeByteOff p (j + 3) (hex (w `mod` 16))
                    fill p (i + 1) (j + 4)
    backslash = 0x5c :: Word8
    hex :: Word8 -> Word8
    hex n
        | n < 10 = 0x30 + n
        | otherwise = 0x57 + n -- lower case as Apache

-- Number of extra bytes needed to escape a byte.
escapeLen :: Word8 -> Int
escapeLen w
    | w == 0x22 || w == 0x5c = 1 -- '"' and '\\'
    | w >= 0x20 && w < 0x7f = 0
    | w >= 0x08 && w <= 0x0d && w /= 0x0c = 1 -- \b \t \n \v \r
    | otherwise = 3

escapeChar :: Word8 -> Word8
escapeChar 0x08 = 0x62 -- b
escapeChar 0x09 = 0x74 -- t
escapeChar 0x0a = 0x6e -- n
escapeChar 0x0b = 0x76 -- v
escapeChar 0x0d = 0x72 -- r
escapeChar w = w -- '"' and '\\'

getSourceIP :: IPAddrSource -> Request -> ByteString
getSourceIP FromSocket = getSourceFromSocket
getSourceIP FromHeader = getSourceFromHeader
getSourceIP FromFallback = getSourceFromFallback
getSourceIP (FromHeaderCustom hs) = fromMaybe "-" . getSourceFromHeaderCustom hs
getSourceIP (FromRequest fromReq) = fromReq

-- |
-- >>> getSourceFromSocket defaultRequest
-- "0.0.0.0"
getSourceFromSocket :: Request -> ByteString
getSourceFromSocket = BS.pack . showSockAddr . remoteHost

-- |
-- >>> getSourceFromHeader defaultRequest { requestHeaders = [ ("X-Real-IP", "127.0.0.1") ] }
-- "127.0.0.1"
-- >>> getSourceFromHeader defaultRequest { requestHeaders = [ ("X-Forwarded-For", "127.0.0.1") ] }
-- "127.0.0.1"
-- >>> getSourceFromHeader defaultRequest { requestHeaders = [ ("Something", "127.0.0.1") ] }
-- "-"
-- >>> getSourceFromHeader defaultRequest { requestHeaders = [] }
-- "-"
--
-- 'getSourceFromHeader' uses the first instance of either @"X-Real-IP"@ or
-- @"X-Forwarded-For"@ that it finds in the ordered header list:
--
-- >>> getSourceFromHeader defaultRequest { requestHeaders = [ ("X-Real-IP", "1.2.3.4"), ("X-Forwarded-For", "5.6.7.8") ] }
-- "1.2.3.4"
-- >>> getSourceFromHeader defaultRequest { requestHeaders = [ ("X-Forwarded-For", "5.6.7.8"), ("X-Real-IP", "1.2.3.4") ] }
-- "5.6.7.8"
--
-- 'getSourceFromHeader' handles pulling out the first IP in the
-- comma-separated IP list in X-Forwarded-For:
--
-- >>> getSourceFromHeader defaultRequest { requestHeaders = [ ("X-Forwarded-For", "5.6.7.8, 10.11.12.13, 1.2.3.4") ] }
-- "5.6.7.8"
getSourceFromHeader :: Request -> ByteString
getSourceFromHeader = fromMaybe "-" . getSource

-- |
-- >>> getSourceFromFallback defaultRequest { requestHeaders = [ ("X-Real-IP", "127.0.0.1") ] }
-- "127.0.0.1"
-- >>> getSourceFromFallback defaultRequest { requestHeaders = [ ("X-Forwarded-For", "127.0.0.1") ] }
-- "127.0.0.1"
-- >>> getSourceFromFallback defaultRequest { requestHeaders = [ ("Something", "127.0.0.1") ] }
-- "0.0.0.0"
-- >>> getSourceFromFallback defaultRequest { requestHeaders = [] }
-- "0.0.0.0"
--
-- 'getSourceFromFallback' uses the first instance of either @"X-Real-IP"@ or
-- @"X-Forwarded-For"@ that it finds in the ordered header list:
--
-- >>> getSourceFromFallback defaultRequest { requestHeaders = [ ("X-Real-IP", "1.2.3.4"), ("X-Forwarded-For", "5.6.7.8") ] }
-- "1.2.3.4"
-- >>> getSourceFromFallback defaultRequest { requestHeaders = [ ("X-Forwarded-For", "5.6.7.8"), ("X-Real-IP", "1.2.3.4") ] }
-- "5.6.7.8"
--
-- 'getSourceFromFallback' handles pulling out the first IP in the
-- comma-separated IP list in X-Forwarded-For:
--
-- >>> getSourceFromFallback defaultRequest { requestHeaders = [ ("X-Forwarded-For", "5.6.7.8, 10.11.12.13, 1.2.3.4") ] }
-- "5.6.7.8"
getSourceFromFallback :: Request -> ByteString
getSourceFromFallback req = fromMaybe (getSourceFromSocket req) $ getSource req

-- |
-- >>> getSource defaultRequest { requestHeaders = [ ("X-Real-IP", "127.0.0.1") ] }
-- Just "127.0.0.1"
-- >>> getSource defaultRequest { requestHeaders = [ ("X-Forwarded-For", "127.0.0.1") ] }
-- Just "127.0.0.1"
-- >>> getSource defaultRequest { requestHeaders = [ ("Something", "127.0.0.1") ] }
-- Nothing
-- >>> getSource defaultRequest
-- Nothing
--
-- 'getSource' uses the first instance of either @"X-Real-IP"@ or
-- @"X-Forwarded-For"@ that it finds in the ordered header list:
--
-- >>> getSource defaultRequest { requestHeaders = [ ("X-Real-IP", "1.2.3.4"), ("X-Forwarded-For", "5.6.7.8") ] }
-- Just "1.2.3.4"
-- >>> getSource defaultRequest { requestHeaders = [ ("X-Forwarded-For", "5.6.7.8"), ("X-Real-IP", "1.2.3.4") ] }
-- Just "5.6.7.8"
--
-- 'getSource' handles pulling out the first IP in the comma-separated IP list
-- in X-Forwarded-For:
--
-- >>> getSource defaultRequest { requestHeaders = [ ("X-Forwarded-For", "5.6.7.8, 10.11.12.13, 1.2.3.4") ] }
-- Just "5.6.7.8"
getSource :: Request -> Maybe ByteString
getSource =
    getSourceFromHeaders
        [("x-real-ip", firstIpInXFF), ("x-forwarded-for", firstIpInXFF)]

-- | Pull out the first IP in a comma-separated list of X-Forwarded-For IPs.
--   Leading white spaces are skipped and the IP ends at a white space.
--
-- >>> firstIpInXFF "1.2.3.4, 5.6.7.8, 10.11.12.13"
-- "1.2.3.4"
-- >>> firstIpInXFF " 1.2.3.4 , 5.6.7.8"
-- "1.2.3.4"
--
-- If there are no commas or white spaces, just return the whole input ByteString:
--
-- >>> firstIpInXFF "5.6.7.8"
-- "5.6.7.8"
--
-- Note that this function doesn't make sure the input is actually an IP address:
--
-- >>> firstIpInXFF "hello, world"
-- "hello"
firstIpInXFF :: ByteString -> ByteString
firstIpInXFF = BS.takeWhile (\c -> c /= ',' && not (isSpace c)) . BS.dropWhile isSpace

getSourceFromHeaders
    :: [(HeaderName, ByteString -> ByteString)] -> Request -> Maybe ByteString
getSourceFromHeaders headerNamesAndPostProc req = getFirst $ foldMap f $ requestHeaders req
  where
    -- Take a header name and value from the request, and try match it against
    -- the list of headers and post-processing functions.  If it matches,
    -- return the ByteString resulting from applying the post-processing function
    -- to the header value.
    f :: (HeaderName, ByteString) -> First ByteString
    f (headerNameFromReq, headerValFromReq) =
        let maybePostProc =
                find
                    (\(headerNameFromPostProc, _) -> headerNameFromReq == headerNameFromPostProc)
                    headerNamesAndPostProc
         in First $ fmap (\(_, postProc) -> postProc headerValFromReq) maybePostProc

-- |
-- >>> getSourceFromHeaderCustom ["x-foobar"] defaultRequest { requestHeaders = [ ("X-catdog", "1.2.3.4"), ("X-Foobar", "5.6.7.8"), ("Other", "1.1.1.1") ] }
-- Just "5.6.7.8"
--
-- If none of the headers in the passed-in list are in the 'Request', then return 'Nothing':
--
-- >>> getSourceFromHeaderCustom ["x-foobar", "baz"] defaultRequest { requestHeaders = [ ("abb", "1.2.3.4"), ("xyz", "5.6.7.8") ] }
-- Nothing
--
-- 'getSourceFromHeaderCustom' uses the first instance of any header in the
-- passed in list that it finds in the ordered header list from the request:
--
-- >>> getSourceFromHeaderCustom ["x-foobar", "baz"] defaultRequest { requestHeaders = [ ("baz", "1.2.3.4"), ("x-foobar", "5.6.7.8") ] }
-- Just "1.2.3.4"
--
-- 'getSourceFromHeaderCustom' splits the value of the header it finds by @,@
-- and uses the first item. This makes it easy to use with headers like
-- @X-Forwarded-For@, which are expected to have a comma-separated list of IP
-- addresses:
--
-- >>> getSourceFromHeaderCustom ["x-foobar"] defaultRequest { requestHeaders = [ ("X-Foobar", "5.6.7.8, 10.11.12.13, 1.2.3.4") ] }
-- Just "5.6.7.8"
getSourceFromHeaderCustom :: [HeaderName] -> Request -> Maybe ByteString
getSourceFromHeaderCustom hs = getSourceFromHeaders (fmap (,firstIpInXFF) hs)
