module RotateSpec where

import Control.Exception (IOException, finally, try)
import Control.Monad (forM, forM_, void)
import qualified Data.ByteString.Char8 as BS
import System.Directory (
    createDirectoryIfMissing,
    doesFileExist,
    removeDirectoryRecursive,
 )
import System.Log.FastLogger
import System.Timeout (timeout)
import Test.Hspec

spec :: Spec
spec = describe "rotate" $ do
    it "shifts the backups and drops the oldest" $
        withFiles ["", ".0", ".1", ".2"] $ do
            rotate (FileLogSpec logFile 0 3)
            contents ["", ".0", ".1", ".2"]
                `shouldReturn` [Nothing, Just "", Just ".0", Just ".1"]
    it "stops shifting at the first missing backup" $
        withFiles ["", ".0", ".2"] $ do
            rotate (FileLogSpec logFile 0 3)
            contents ["", ".0", ".1", ".2"]
                `shouldReturn` [Nothing, Just "", Just ".0", Just ".2"]
    it "does not blow up with maxBound backups (#212)" $
        withFiles ["", ".0"] $ do
            timeout 3000000 (rotate (FileLogSpec logFile 0 maxBound))
                `shouldReturn` Just ()
            contents ["", ".0", ".1"]
                `shouldReturn` [Nothing, Just "", Just ".0"]

dir :: FilePath
dir = "test/rotate"

logFile :: FilePath
logFile = dir ++ "/log"

-- Each file holds its own suffix so that moves can be traced.
withFiles :: [String] -> IO () -> IO ()
withFiles sfxs act = flip finally cleanup $ do
    cleanup
    createDirectoryIfMissing True dir
    forM_ sfxs $ \s -> writeFile (logFile ++ s) s
    act
  where
    cleanup =
        void (try (removeDirectoryRecursive dir) :: IO (Either IOException ()))

contents :: [String] -> IO [Maybe String]
contents sfxs = forM sfxs $ \s -> do
    let f = logFile ++ s
    exist <- doesFileExist f
    if exist then Just . BS.unpack <$> BS.readFile f else return Nothing
