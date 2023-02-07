{-# LANGUAGE OverloadedStrings, BangPatterns, CPP #-}

module FastLoggerSpec (spec) where

#if __GLASGOW_HASKELL__ < 709
import Control.Applicative ((<$>))
#endif
import Control.Exception (finally)
import Control.Concurrent
import Control.Concurrent.Async
import Control.Monad
import qualified Data.ByteString.Char8 as BS
import Data.List (sort)
#if !MIN_VERSION_base(4,11,0)
import Data.Monoid ((<>))
#endif
import Data.String (IsString(fromString))
import System.Directory
import System.Log.FastLogger
import Test.Hspec
import Test.Hspec.QuickCheck (prop)
import Text.Printf

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

  describe "fastlogger 1" $ do
    it "maintains the ordering of log messages" $ do
        let tmpfile = "/tmp/fastlogger-test.txt"
        cleanup tmpfile
        (pushlog, teardown) <- newFastLogger1 $ LogFileNoRotate tmpfile 128
        numCapabilities <- getNumCapabilities
        let concurrency = numCapabilities * 200 :: Int
            logEntriesCount = 100 :: Int
        forConcurrently_ [0 .. concurrency - 1] $ \t ->
          forM_ [0 .. logEntriesCount - 1] $ \i -> do
            let tag = "thread id: " <> show t <> " " :: String
                cnt = printf "%02d" i :: String
                logmsg = toLogStr tag <> "log line nr: " <> toLogStr cnt <> "\n"
            pushlog logmsg
        teardown
        xs <- BS.lines <$> BS.readFile tmpfile
        forM_ [0 .. concurrency - 1] $ \t -> do
            let tag = BS.pack ("thread id: " <> show t <> " ")
                msgs = filter (tag `BS.isPrefixOf`) xs
            sort msgs `shouldBe` msgs
        cleanup tmpfile


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
