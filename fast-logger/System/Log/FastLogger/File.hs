{-# LANGUAGE Safe #-}

module System.Log.FastLogger.File
    ( FileLogSpec(..)
    , DailyFileLogSpec (..)
    , check
    , rotate
    , timedRotate
    ) where

import Control.Concurrent (forkIO)
import Control.Monad (unless, when, void)
import Data.ByteString (ByteString)
import Data.ByteString.Char8 (unpack)
import System.Directory (doesFileExist, doesDirectoryExist, getPermissions, writable, renameFile)
import System.FilePath (takeDirectory, dropFileName, takeFileName, (</>))

type FormattedTime = ByteString -- redeclaration to allow for LANGUAGE Safe

-- | The spec for logging files
data FileLogSpec = FileLogSpec {
    log_file :: FilePath
  , log_file_size :: Integer -- ^ Max log file size (in bytes) before requiring rotation.
  , log_backup_number :: Int -- ^ Max number of rotated log files to keep around before overwriting the oldest one.
  }

-- | The spec for daily rotation base. It supports post processing of log files
-- and will consider any file with an valid ISO 8601 format timestring and the
-- daily_log_file specifier as part of the rotation.
data DailyFileLogSpec = DailyFileLogSpec {
    daily_log_file :: FilePath
  , daily_post_process :: FilePath -> IO () -- ^ processing function called asynchronously after a file is added to the rotation
  }

-- | Checking if a log file can be written.
check :: FilePath -> IO ()
check file = do
    dirExist <- doesDirectoryExist dir
    unless dirExist $ fail $ dir ++ " does not exist or is not a directory."
    dirPerm <- getPermissions dir
    unless (writable dirPerm) $ fail $ dir ++ " is not writable."
    exist <- doesFileExist file
    when exist $ do
        perm <- getPermissions file
        unless (writable perm) $ fail $ file ++ " is not writable."
  where
    dir = takeDirectory file

-- | Rotating log files.
rotate :: FileLogSpec -> IO ()
rotate spec = mapM_ move srcdsts
  where
    path = log_file spec
    n = log_backup_number spec
    dsts' = reverse . ("":) . map (('.':). show) $ [0..n-1]
    dsts = map (path++) dsts'
    srcs = tail dsts
    srcdsts = zip srcs dsts
    move (src,dst) = do
        exist <- doesFileExist src
        when exist $ renameFile src dst

-- | Rotating log files based on time.
timedRotate :: DailyFileLogSpec -> FormattedTime -> IO ()
timedRotate spec oldTime = do
    move (path, dropFileName path </> unpack oldTime ++ "_" ++ takeFileName path)
    void $ forkIO $ daily_post_process spec path
  where
    path = daily_log_file spec
    move (src,dst) = do
        exist <- doesFileExist src
        when exist $ renameFile src dst
