{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}

module FastLoggerSpec (spec) where

#if __GLASGOW_HASKELL__ < 709
import Control.Applicative ((<$>))
#endif
import Control.Concurrent (getNumCapabilities)
import Control.Concurrent.Async (forConcurrently_)
import Control.Exception (finally)
import Control.Monad (forM_, when)
import qualified Data.ByteString.Char8 as BS
import Data.List (sort)
#if !MIN_VERSION_base(4,11,0)
import Data.Monoid ((<>))
#endif
import Data.String (IsString (fromString))
import System.Directory (doesFileExist, removeFile)
import Text.Printf (printf)

import Test.Hspec
import Test.Hspec.QuickCheck (prop)

import System.Log.FastLogger

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
        it "is safe for a large message" $
            safeForLarge
                [ 100
                , 1000
                , 10000
                , 100000
                , 1000000
                ]
        it "logs all messages" logAllMsgs

    describe "fastlogger 1" $ do
        it "maintains the ordering of log messages" logOrdering

tempFile :: FilePath
tempFile = "test/temp.txt"

safeForLarge :: [Int] -> IO ()
safeForLarge = mapM_ safeForLarge'

safeForLarge' :: Int -> IO ()
safeForLarge' n = flip finally (cleanup tempFile) $ do
    cleanup tempFile
    lgrset <- newFileLoggerSet defaultBufSize tempFile
    let xs = toLogStr $ BS.pack $ take (abs n) (cycle ['a' .. 'z'])
        lf = "x"
    pushLogStr lgrset $ xs <> lf
    flushLogStr lgrset
    rmLoggerSet lgrset
    bs <- BS.readFile tempFile
    bs `shouldBe` BS.pack (take (abs n) (cycle ['a' .. 'z']) <> "x")

cleanup :: FilePath -> IO ()
cleanup file = do
    exist <- doesFileExist file
    when exist $ removeFile file

logAllMsgs :: IO ()
logAllMsgs = logAll "LICENSE" `finally` cleanup tempFile
  where
    logAll file = do
        cleanup tempFile
        lgrset <- newFileLoggerSet 512 tempFile
        src <- BS.readFile file
        let bs = (<> "\n") . toLogStr <$> BS.lines src
        mapM_ (pushLogStr lgrset) bs
        flushLogStr lgrset
        rmLoggerSet lgrset
        dst <- BS.readFile tempFile
        dst `shouldBe` src

logOrdering :: IO ()
logOrdering = flip finally (cleanup tempFile) $ do
    cleanup tempFile
    -- 128 is small enough for out-of-ordering
    (pushlog, teardown) <- newFastLogger1 $ LogFileNoRotate tempFile 128
    numCapabilities <- getNumCapabilities
    let concurrency = numCapabilities * 200 :: Int
        logEntriesCount = 100 :: Int
    forConcurrently_ [0 .. concurrency - 1] $ \t ->
        forM_ [0 .. logEntriesCount - 1] $ \i -> do
            let tag = mktag t
                cnt = printf "%02d" i :: String
                logmsg = toLogStr tag <> "log line nr: " <> toLogStr cnt <> "\n"
            pushlog logmsg
    teardown
    xs <- BS.lines <$> BS.readFile tempFile
    forM_ [0 .. concurrency - 1] $ \t -> do
        let tag = BS.pack $ mktag t
            msgs = filter (tag `BS.isPrefixOf`) xs
        sort msgs `shouldBe` msgs
  where
    mktag :: Int -> String
    mktag t = "thread id: " <> show t <> " "
