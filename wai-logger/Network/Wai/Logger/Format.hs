{-# LANGUAGE OverloadedStrings #-}

module Network.Wai.Logger.Format (
    IPAddrSource(..)
  , apacheFormat
  -- * Builder
  , apacheFormatBuilder
  ) where

import Blaze.ByteString.Builder
import Blaze.ByteString.Builder.Char8
import Data.ByteString.Char8 (ByteString)
import Data.CaseInsensitive
import Data.List
import Data.Maybe
import Data.Monoid
import Network.HTTP.Types
import Network.Wai
import Network.Wai.Logger.Utils
import System.Log.FastLogger

-- $setup
-- >>> :set -XOverloadedStrings
-- >>> import Network.Wai.Test

{-| Source from which the IP source address of the client is obtained.
-}
data IPAddrSource =
  -- | From the peer address of the HTTP connection.
    FromSocket
  -- | From X-Real-IP: or X-Forwarded-For: in the HTTP header.
  | FromHeader
  -- | From the peer address if header is not found.
  | FromFallback

-- | Apache style log format.
apacheFormat :: IPAddrSource -> ZonedDate -> Request -> Status -> Maybe Integer -> [LogStr]
apacheFormat ipsrc tmstr req st msize = [
    getSourceIPLogStr ipsrc req
  , LB " - - ["
  , LB tmstr
  , LB "] \""
  , LB $ requestMethod req
  , LB " "
  , LB $ rawPathInfo req
  , LB " "
  , LS $ show . httpVersion $ req
  , LB "\" "
  , LS . show . statusCode $ st
  , LB " "
  , LS $ maybe "-" show msize
  , LB " \""
  , LB $ lookupRequestField' "referer" req
  , LB "\" \""
  , LB $ lookupRequestField' "user-agent" req
  , LB "\"\n"
  ]

{-| Apache style log format with 'Builder'. This is experimental.
    This would replace 'apacheFormat' someday.
-}
apacheFormatBuilder :: IPAddrSource -> ZonedDate -> Request -> Status -> Maybe Integer -> Builder
apacheFormatBuilder ipsrc tmstr req status msize =
      getSourceIPBuilder ipsrc req
  +++ bs " - - ["
  +++ bs tmstr
  +++ bs "] \""
  +++ bs (requestMethod req)
  +++ bs " "
  +++ bs (rawPathInfo req)
  +++ bs " "
  +++ st (show (httpVersion req))
  +++ bs "\" "
  +++ st (show (statusCode status))
  +++ bs " "
  +++ st (maybe "-" show msize)
  +++ bs " \""
  +++ bs (lookupRequestField' "referer" req)
  +++ bs "\" \""
  +++ bs (lookupRequestField' "user-agent" req)
  +++ bs "\"\n"
  where
    st = fromString
    bs = fromByteString
    (+++) = mappend

lookupRequestField' :: CI ByteString -> Request -> ByteString
lookupRequestField' k req = fromMaybe "" . lookup k $ requestHeaders req

getSourceIPLogStr :: IPAddrSource -> Request -> LogStr
getSourceIPLogStr = getSourceIP LS LB

getSourceIPBuilder :: IPAddrSource -> Request -> Builder
getSourceIPBuilder = getSourceIP fromString fromByteString

getSourceIP :: (String -> a) -> (ByteString -> a) -> IPAddrSource -> Request -> a
getSourceIP f _ FromSocket = f . getSourceFromSocket
getSourceIP _ g FromHeader = g . getSourceFromHeader
getSourceIP f g FromFallback = either f g . getSourceFromFallback

-- |
-- >>> getSourceFromSocket defaultRequest
-- "0.0.0.0"
getSourceFromSocket :: Request -> String
getSourceFromSocket = showSockAddr . remoteHost

-- |
-- >>> getSourceFromHeader defaultRequest { requestHeaders = [ ("X-Real-IP", "127.0.0.1") ] }
-- "127.0.0.1"
-- >>> getSourceFromHeader defaultRequest { requestHeaders = [ ("X-Forwarded-For", "127.0.0.1") ] }
-- "127.0.0.1"
-- >>> getSourceFromHeader defaultRequest { requestHeaders = [ ("Something", "127.0.0.1") ] }
-- ""
-- >>> getSourceFromHeader defaultRequest { requestHeaders = [] }
-- ""
getSourceFromHeader :: Request -> ByteString
getSourceFromHeader = fromMaybe "" . getSource

-- |
-- >>> getSourceFromFallback defaultRequest { requestHeaders = [ ("X-Real-IP", "127.0.0.1") ] }
-- Right "127.0.0.1"
-- >>> getSourceFromFallback defaultRequest { requestHeaders = [ ("X-Forwarded-For", "127.0.0.1") ] }
-- Right "127.0.0.1"
-- >>> getSourceFromFallback defaultRequest { requestHeaders = [ ("Something", "127.0.0.1") ] }
-- Left "0.0.0.0"
-- >>> getSourceFromFallback defaultRequest { requestHeaders = [] }
-- Left "0.0.0.0"
getSourceFromFallback :: Request -> Either String ByteString
getSourceFromFallback req = maybe (Left $ getSourceFromSocket req) Right $ getSource req

-- |
-- >>> getSource defaultRequest { requestHeaders = [ ("X-Real-IP", "127.0.0.1") ] }
-- Just "127.0.0.1"
-- >>> getSource defaultRequest { requestHeaders = [ ("X-Forwarded-For", "127.0.0.1") ] }
-- Just "127.0.0.1"
-- >>> getSource defaultRequest { requestHeaders = [ ("Something", "127.0.0.1") ] }
-- Nothing
-- >>> getSource defaultRequest
-- Nothing
getSource :: Request -> Maybe ByteString
getSource req = addr
  where
    maddr = find (\x -> fst x `elem` ["x-real-ip", "x-forwarded-for"]) hdrs
    addr = fmap snd maddr
    hdrs = requestHeaders req
