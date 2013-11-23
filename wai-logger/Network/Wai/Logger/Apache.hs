{-# LANGUAGE OverloadedStrings #-}

module Network.Wai.Logger.Apache (
    IPAddrSource(..)
  , apacheLogMsg
  ) where

import Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Char8 as BS
import Data.CaseInsensitive
import Data.List
import Data.Maybe
import Data.Monoid
import Network.HTTP.Types
import Network.Wai
import Network.Wai.Logger.Date
import Network.Wai.Logger.IP
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
apacheLogMsg :: IPAddrSource -> ZonedDate -> Request -> Status -> Maybe Integer -> LogMsg
apacheLogMsg ipsrc tmstr req status msize =
      bs (getSourceIP ipsrc req)
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
    st = bs . BS.pack
    bs = fromByteString
    (+++) = mappend

lookupRequestField' :: CI ByteString -> Request -> ByteString
lookupRequestField' k req = fromMaybe "" . lookup k $ requestHeaders req

-- getSourceIP = getSourceIP fromString fromByteString

getSourceIP :: IPAddrSource -> Request -> ByteString
getSourceIP FromSocket   = getSourceFromSocket
getSourceIP FromHeader   = getSourceFromHeader
getSourceIP FromFallback = getSourceFromFallback

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
getSource :: Request -> Maybe ByteString
getSource req = addr
  where
    maddr = find (\x -> fst x `elem` ["x-real-ip", "x-forwarded-for"]) hdrs
    addr = fmap snd maddr
    hdrs = requestHeaders req
