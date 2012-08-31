module FastLoggerSpec where

import Control.Exception (bracket)
import System.IO
import System.Log.FastLogger
import Test.Hspec

spec :: Spec
spec = describe "hPutLogStr" $ do
    it "is safe for a large message" $ safeForLarge [
        100
      , 1000
      , 10000
      , 100000
      , 1000000
      ]

nullLogger :: IO Logger
nullLogger = openFile "/dev/null" WriteMode >>= mkLogger True

safeForLarge :: [Int] -> IO ()
safeForLarge ns = mapM_ safeForLarge' ns

safeForLarge' :: Int -> IO ()
safeForLarge' n = bracket nullLogger rmLogger $ \lgr ->
    loggerPutStr lgr [LS $ replicate (abs n) 'x']
