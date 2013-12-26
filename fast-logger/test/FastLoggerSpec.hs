{-# LANGUAGE OverloadedStrings #-}

module FastLoggerSpec where

import Control.Exception (bracket)
import qualified Data.ByteString.Char8 as BS
import Data.Monoid ((<>))
import System.Log.FastLogger
import Test.Hspec

spec :: Spec
spec = describe "pushLogMsg" $ do
    it "is safe for a large message" $ safeForLarge [
        100
      , 1000
      , 10000
      , 100000
      , 1000000
      ]

nullLogger :: IO LoggerSet
nullLogger = newLoggerSet 4096 "/dev/null"

safeForLarge :: [Int] -> IO ()
safeForLarge ns = mapM_ safeForLarge' ns

safeForLarge' :: Int -> IO ()
safeForLarge' n = bracket nullLogger rmLoggerSet $ \lgrset -> do
    let xs = toLogStr $ BS.pack $ replicate (abs n) 'x'
        lf = "x"
    pushLogStr lgrset $ xs <> lf
    flushLogStr lgrset
