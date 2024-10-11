{-# LANGUAGE OverloadedStrings, CPP, TupleSections #-}

module Network.Wai.Logger.Apache (
    IPAddrSource(..)
  , apacheLogStr
  , serverpushLogStr
  ) where

#ifndef MIN_VERSION_base
#define MIN_VERSION_base(x,y,z) 1
#endif
#ifndef MIN_VERSION_wai
#define MIN_VERSION_wai(x,y,z) 1
#endif

import Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Char8 as BS
import Data.List (find)
import Data.Maybe (fromMaybe)
#if MIN_VERSION_base(4,5,0)
import Data.Monoid ((<>), First (..))
#else
import Data.Monoid (mappend)
#endif
import Network.HTTP.Types (Status, statusCode)
import Network.HTTP.Types.Header (HeaderName)
import Network.Wai (Request(..))
import Network.Wai.Logger.IP
import System.Log.FastLogger

-- $setup
-- >>> :set -XOverloadedStrings
-- >>> import Network.Wai (defaultRequest)

-- | Source from which the IP source address of the client is obtained.
data IPAddrSource =
  -- | From the peer address of the HTTP connection.
    FromSocket
  -- | From X-Real-IP: or X-Forwarded-For: in the HTTP header.
  --
  -- This picks either X-Real-IP or X-Forwarded-For depending on which of these
  -- headers comes first in the ordered list of request headers.
  --
  -- If the X-Forwarded-For header is picked, the value will be parsed and the
  -- left-most IP address will be used (which is mostly likely to be the actual
  -- client IP address).
  | FromHeader
  -- | From a custom HTTP header, useful in proxied environment.
  | FromHeaderCustom [HeaderName]
  -- | Just like 'FromHeader', but falls back on the peer address if header is not found.
  | FromFallback
  -- | This gives you the most flexibility to figure out the IP source address
  -- from the 'Request'.  The returned 'ByteString' is used as the IP source
  -- address.
  | FromRequest (Request -> ByteString)

-- | Apache style log format.
apacheLogStr :: ToLogStr user => IPAddrSource -> (Request -> Maybe user) -> FormattedTime -> Request -> Status -> Maybe Integer -> LogStr
apacheLogStr ipsrc userget tmstr req status msize =
      toLogStr (getSourceIP ipsrc req)
  <> " - "
  <> maybe "-" toLogStr (userget req)
  <> " ["
  <> toLogStr tmstr
  <> "] \""
  <> toLogStr (requestMethod req)
  <> " "
  <> toLogStr path
  <> " "
  <> toLogStr (show (httpVersion req))
  <> "\" "
  <> toLogStr (show (statusCode status))
  <> " "
  <> toLogStr (maybe "-" show msize)
  <> " \""
  <> toLogStr (fromMaybe "" mr)
  <> "\" \""
  <> toLogStr (fromMaybe "" mua)
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
serverpushLogStr :: ToLogStr user => IPAddrSource -> (Request -> Maybe user) -> FormattedTime -> Request -> ByteString -> Integer -> LogStr
serverpushLogStr ipsrc userget tmstr req path size =
      toLogStr (getSourceIP ipsrc req)
  <> " - "
  <> maybe "-" toLogStr (userget req)
  <> " ["
  <> toLogStr tmstr
  <> "] \"PUSH "
  <> toLogStr path
  <> " HTTP/2\" 200 "
  <> toLogStr (show size)
  <> " \""
  <> toLogStr ref
  <> "\" \""
  <> toLogStr (fromMaybe "" mua)
  <> "\"\n"
  where
    ref  = rawPathInfo req
#if !MIN_VERSION_base(4,5,0)
    (<>) = mappend
#endif
#if MIN_VERSION_wai(3,2,0)
    mua = requestHeaderUserAgent req
#else
    mua = lookup "user-agent" $ requestHeaders req
#endif

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
getSource = getSourceFromHeaders [("x-real-ip", id), ("x-forwarded-for", firstIpInXFF)]

-- | Pull out the first IP in a comma-separated list of X-Forwarded-For IPs.
--
-- >>> firstIpInXFF "1.2.3.4, 5.6.7.8, 10.11.12.13"
-- "1.2.3.4"
--
-- If there are no commas, just return the whole input ByteString:
--
-- >>> firstIpInXFF "5.6.7.8"
-- "5.6.7.8"
--
-- Note that this function doesn't make sure the input is actually an IP address:
--
-- >>> firstIpInXFF "hello, world"
-- "hello"
firstIpInXFF :: ByteString -> ByteString
firstIpInXFF = BS.takeWhile (/= ',')

getSourceFromHeaders :: [(HeaderName, ByteString -> ByteString)] -> Request -> Maybe ByteString
getSourceFromHeaders headerNamesAndPostProc req = getFirst $ foldMap f $ requestHeaders req
  where
    -- Take a header name and value from the request, and try match it against
    -- the list of headers and post-processing functions.  If it matches,
    -- return the ByteString resulting from applying the post-processing function
    -- to the header value.
    f :: (HeaderName, ByteString) -> First ByteString
    f (headerNameFromReq, headerValFromReq) =
      let maybePostProc = find (\(headerNameFromPostProc, _) -> headerNameFromReq == headerNameFromPostProc) headerNamesAndPostProc
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
getSourceFromHeaderCustom :: [HeaderName] -> Request -> Maybe ByteString
getSourceFromHeaderCustom hs = getSourceFromHeaders (fmap (,id) hs)
