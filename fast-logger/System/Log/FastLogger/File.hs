{-# LANGUAGE Safe #-}

module System.Log.FastLogger.File
    ( FileLogSpec(..)
    , TimedFileLogSpec(..)
    , check
    , rotate
    , timedRotate
    ) where

import Control.Monad (unless, when)
import Data.ByteString (ByteString)
import Data.ByteString.Char8 (unpack)
import System.Directory (doesFileExist, doesDirectoryExist, getPermissions, writable, renameFile, removeFile)
import System.FilePath (takeDirectory)

type TimeFormat = ByteString -- redeclaration to allow for LANGUAGE Safe
type FormattedTime = ByteString -- redeclaration to allow for LANGUAGE Safe

-- | The spec for logging files
data FileLogSpec = FileLogSpec {
    log_file :: FilePath
  , log_file_size :: Integer -- ^ Max log file size (in bytes) before requiring rotation.
  , log_backup_number :: Int -- ^ Max number of rotated log files to keep around before overwriting the oldest one.
  }

-- | The spec for rotation base on timeframe
-- Note: This keeps track of former filenames to do the rotation. This means
-- that old log files will be ignored after a restart of the application.
data TimedFileLogSpec = TimedFileLogSpec {
    timed_log_file :: FilePath
  , timed_log_format :: TimeFormat -- ^ Format that will cause a rotation whenever changed
                                   --   You can use e.g. "%y" to get a yearly
                                   --   rotation, "%d" (or "%m/%d/%y") to get a
                                   --   daily rotation. This uses a time cache
                                   --   for performance reasons, so anything
                                   --   less than a minutely rotation will not
                                   --   work reliably.
  , timed_log_backup_number :: Int -- ^ Max number of rotated log files to keep around before overwriting the oldest one.
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
timedRotate :: TimedFileLogSpec -> [FormattedTime] -> IO ()
timedRotate spec times = do
    move (path, newest)
    when (length times > number) $ remove oldest
  where
    path = timed_log_file spec
    oldest = path ++ '.' : unpack (last times)
    newest = path ++ '.' : unpack (head times)

    number = timed_log_backup_number spec
    remove path_ = do
        exists <- doesFileExist path_
        when exists $ removeFile path_
    move (src,dst) = do
        exist <- doesFileExist src
        when exist $ renameFile src dst
