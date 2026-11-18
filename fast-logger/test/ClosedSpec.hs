module ClosedSpec where

import Control.Exception (finally)
import System.Directory (doesFileExist, removeFile)
import System.IO.Error (isIllegalOperation)
import System.Log.FastLogger
import System.Timeout (timeout)
import Test.Hspec

spec :: Spec
spec = describe "removed LoggerSet (#166)" $ do
    it "fails only on pushing with the single logger" $ closed (Just 1)
    it "fails only on pushing with the multi logger" $ closed Nothing

closed :: Maybe Int -> IO ()
closed mn = flip finally cleanup $ do
    cleanup
    lgrset <- newFileLoggerSetN 64 mn logFile
    rmLoggerSet lgrset
    pushLogStr lgrset (toLogStr "after removal\n") `shouldThrow` isIllegalOperation
    timeout 3000000 (flushLogStr lgrset) `shouldReturn` Just ()
    timeout 3000000 (rmLoggerSet lgrset) `shouldReturn` Just ()
    readFile logFile `shouldReturn` ""
  where
    cleanup = do
        exist <- doesFileExist logFile
        if exist then removeFile logFile else return ()

logFile :: FilePath
logFile = "test/closed.txt"
