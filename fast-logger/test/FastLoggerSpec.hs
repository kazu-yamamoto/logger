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
nullLogger = logOpen "/dev/null" >>= newLoggerSet 4096

safeForLarge :: [Int] -> IO ()
safeForLarge ns = mapM_ safeForLarge' ns

safeForLarge' :: Int -> IO ()
safeForLarge' n = bracket nullLogger rmLoggerSet $ \lgrset -> do
    let xs = fromByteString $ BS.pack $ replicate (abs n) 'x'
        lf = fromByteString "x"
    pushLogMsg lgrset $ xs <> lf
    flushLogMsg lgrset
