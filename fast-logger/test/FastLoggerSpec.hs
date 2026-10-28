{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}

module FastLoggerSpec (spec) where

#if __GLASGOW_HASKELL__ < 709
import Control.Applicative ((<$>))
#endif
import Control.Concurrent (getNumCapabilities)
import Control.Concurrent.Async (concurrently, forConcurrently_)
import Control.Exception (finally)
import Control.Monad (forM_, when)
import qualified Data.ByteString.Char8 as BS
import Data.IORef (newIORef, readIORef, writeIORef)
import Data.List (sort)
#if !MIN_VERSION_base(4,11,0)
import Data.Monoid ((<>))
#endif
import Data.String (IsString (fromString))
import System.Directory (doesFileExist, removeFile, renameFile)
import System.Timeout (timeout)
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

    describe "renewLoggerSet" $ do
        it "does not close the FD under the single logger's writer" $
            renewWhileLogging (Just 1)
        it "does not close the FD under the multi logger's writers" $
            renewWhileLogging Nothing

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

-- Rotates the way 'tryRotate' does while messages are being written.
-- An FD closed under a writer makes a write fail with EBADF.
renewWhileLogging :: Maybe Int -> IO ()
#if defined(mingw32_HOST_OS)
-- An open file cannot be renamed under the POSIX I/O manager.
renewWhileLogging _ = pendingWith "renaming an open file"
#else
renewWhileLogging mn = flip finally (mapM_ cleanup files) $ do
    mapM_ cleanup files
    lgrset <- newFileLoggerSetN 64 mn tempFile
    renewed <- newIORef False
    let logger i = do
            pushLogStr lgrset (toLogStr (show i) <> "\n")
            fin <- readIORef renewed
            if fin && i >= 20000 then return i else logger (i + 1)
        renewer = do
            forM_ backups $ \backup -> do
                renameFile tempFile backup
                renewLoggerSet lgrset
            writeIORef renewed True
    res <- timeout 30000000 $ do
        (pushed, _) <- concurrently (logger (1 :: Int)) renewer
        flushLogStr lgrset
        rmLoggerSet lgrset
        return pushed
    case res of
        Nothing -> expectationFailure "flushing or removing the logger set hung"
        Just pushed -> do
            xs <- concatMap BS.lines <$> mapM BS.readFile files
            length xs `shouldBe` pushed
  where
    backups = [tempFile <> "." <> show i | i <- [1 .. 500 :: Int]]
    files = tempFile : backups
#endif
