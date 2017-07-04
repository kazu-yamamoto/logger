{-# LANGUAGE OverloadedStrings, BangPatterns, CPP #-}

module FastLoggerSpec where

#if __GLASGOW_HASKELL__ < 709
import Control.Applicative ((<$>))
#endif
import Control.Exception (finally)
import Control.Monad (when)
import qualified Data.ByteString.Char8 as BS
import Data.Monoid ((<>))
import Data.String (IsString(fromString))
import System.Directory (doesFileExist, removeFile)
import System.Log.FastLogger
import Test.Hspec
import Test.Hspec.QuickCheck (prop)

spec :: Spec
spec = do
  describe "instance Show LogStr" $ do
    prop "it should be consistent with instance IsString" $ \str ->
      let logstr :: LogStr
          logstr = fromString str
      in show logstr == show str
  describe "instance Eq LogStr" $ do
    prop "it should be consistent with instance IsString" $ \str1 str2 ->
      let logstr1, logstr2 :: LogStr
          logstr1 = fromString str1
          logstr2 = fromString str2
      in (logstr1 == logstr2) == (str1 == str2)
  describe "pushLogMsg" $ do
    it "is safe for a large message" $ safeForLarge [
        100
      , 1000
      , 10000
      , 100000
      , 1000000
      ]
    it "logs all messages" logAllMsgs

nullLogger :: IO LoggerSet
#ifdef mingw32_HOST_OS
nullLogger = newFileLoggerSet 4096 "nul"
#else
nullLogger = newFileLoggerSet 4096 "/dev/null"
#endif

safeForLarge :: [Int] -> IO ()
safeForLarge ns = mapM_ safeForLarge' ns

safeForLarge' :: Int -> IO ()
safeForLarge' n = flip finally (cleanup tmpfile) $ do
    cleanup tmpfile
    lgrset <- newFileLoggerSet defaultBufSize tmpfile
    let xs = toLogStr $ BS.pack $ take (abs n) (cycle ['a'..'z'])
        lf = "x"
    pushLogStr lgrset $ xs <> lf
    flushLogStr lgrset
    rmLoggerSet lgrset
    bs <- BS.readFile tmpfile
    bs `shouldBe` BS.pack (take (abs n) (cycle ['a'..'z']) <> "x")
    where
        tmpfile = "test/temp"

cleanup :: FilePath -> IO ()
cleanup file = do
    exist <- doesFileExist file
    when exist $ removeFile file

logAllMsgs :: IO ()
logAllMsgs = logAll "LICENSE" `finally` cleanup tmpfile
  where
    tmpfile = "test/temp"
    logAll file = do
        cleanup tmpfile
        lgrset <- newFileLoggerSet 512 tmpfile
        src <- BS.readFile file
        let bs = (<> "\n") . toLogStr <$> BS.lines src
        mapM_ (pushLogStr lgrset) bs
        flushLogStr lgrset
        rmLoggerSet lgrset
        dst <- BS.readFile tmpfile
        dst `shouldBe` src
