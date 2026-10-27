{-# LANGUAGE OverloadedStrings #-}

module ApacheSpec (spec) where

import qualified Data.ByteString.Char8 as BS
import Data.IORef
import Network.HTTP.Types (status200)
import Network.Wai
import Network.Wai.Logger
import System.Log.FastLogger

import Test.Hspec

spec :: Spec
spec = describe "apacheLogger" $ do
    it "escapes request-derived fields" $ do
        let req =
                defaultRequest
                    { rawPathInfo = "/a\"b"
                    , requestHeaders = [("X-Forwarded-For", " 1.2.3.4 \"x\"\n, 5.6.7.8")]
                    , requestHeaderUserAgent = Just "evil\" \"injected\r\nfoo\\"
                    , requestHeaderReferer = Just "\x01\x7f"
                    }
        logLine FromHeader req
            `shouldReturn` "1.2.3.4 - - [date] \"GET /a\\\"b HTTP/1.0\" 200 4 \"\\x01\\x7f\" \"evil\\\" \\\"injected\\r\\nfoo\\\\\"\n"
    it "does not change ordinary fields" $ do
        let req =
                defaultRequest
                    { rawPathInfo = "/index.html"
                    , rawQueryString = "?q=1"
                    , requestHeaders = [("X-Real-IP", "9.8.7.6")]
                    , requestHeaderUserAgent = Just "Mozilla/5.0 (X11)"
                    }
        logLine FromHeader req
            `shouldReturn` "9.8.7.6 - - [date] \"GET /index.html?q=1 HTTP/1.0\" 200 4 \"\" \"Mozilla/5.0 (X11)\"\n"

logLine :: IPAddrSource -> Request -> IO BS.ByteString
logLine ipsrc req = do
    ref <- newIORef ""
    let cb s = modifyIORef ref (<> fromLogStr s)
    lgr <- initLogger ipsrc (LogCallback cb (return ())) (return "date")
    apacheLogger lgr req status200 (Just 4)
    readIORef ref
