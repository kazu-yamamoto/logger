{-# LANGUAGE OverloadedStrings #-}

module Network.Wai.Logger.Format (
    IPAddrSource(..)
  , apacheFormat
  -- * Builder
  , apacheFormatBuilder
  ) where

import Blaze.ByteString.Builder
import Blaze.ByteString.Builder.Char8
import Data.ByteString.Char8 ()
import Data.CaseInsensitive
import Data.List
import Data.Maybe
import Data.Monoid
import Network.HTTP.Types
import Network.Wai
import Network.Wai.Logger.Date
import Network.Wai.Logger.Utils
import System.Log.FastLogger

{-| Source from which the IP source address of the client is obtained.
-}
data IPAddrSource =
  -- | From the peer address of the HTTP connection.
    FromSocket
  -- | From X-Real-IP: or X-Forwarded-For: in the HTTP header.
  | FromHeader

-- | Apache style log format.
apacheFormat :: IPAddrSource -> ZonedDate -> Request -> Status -> Maybe Integer -> [LogStr]
apacheFormat ipsrc tmstr req st msize = [
    getSourceIP ipsrc req
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
      getSourceIP' ipsrc req
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

lookupRequestField' :: CI Ascii -> Request -> Ascii
lookupRequestField' k req = fromMaybe "" . lookup k $ requestHeaders req

getSourceIP :: IPAddrSource -> Request -> LogStr
getSourceIP FromSocket = LS . showSockAddr . remoteHost
getSourceIP FromHeader = LB . getSource

getSourceIP' :: IPAddrSource -> Request -> Builder
getSourceIP' FromSocket = fromString . showSockAddr . remoteHost
getSourceIP' FromHeader = fromByteString . getSource

getSource :: Request -> Ascii
getSource req = addr
  where
    maddr = find (\x -> fst x `elem` ["x-real-ip", "x-forwarded-for"]) hdrs
    addr = maybe "" snd maddr
    hdrs = requestHeaders req
