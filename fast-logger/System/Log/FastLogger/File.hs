{-# LANGUAGE Safe #-}

module System.Log.FastLogger.File
    ( FileLogSpec(..)
    , TimedFileLogSpec (..)
    , check
    , rotate
    , prefixTime
    ) where

import Control.Monad (unless, when)
import Data.ByteString (ByteString)
import Data.ByteString.Char8 (unpack)
import System.Directory (doesFileExist, doesDirectoryExist, getPermissions, writable, renameFile)
import System.FilePath (takeDirectory, dropFileName, takeFileName, (</>))

type TimeFormat = ByteString -- redeclaration to allow for LANGUAGE Safe
type FormattedTime = ByteString -- redeclaration to allow for LANGUAGE Safe

-- | The spec for logging files
data FileLogSpec = FileLogSpec {
    log_file :: FilePath
  , log_file_size :: Integer -- ^ Max log file size (in bytes) before requiring rotation.
  , log_backup_number :: Int -- ^ Max number of rotated log files to keep around before overwriting the oldest one.
  }

-- | The spec for time based rotation. It supports post processing of log files. Does
-- not delete any logs.
data TimedFileLogSpec = TimedFileLogSpec {
    timed_log_file :: FilePath              -- ^ base file path
  , timed_timefmt  :: TimeFormat            -- ^ time format to prepend
  , timed_same_timeframe  :: FormattedTime -> FormattedTime -> Bool
                                            -- ^ function that compares two
                                            --   formatted times as specified by
                                            --   timed_timefmt and decides if a
                                            --   new rotation is supposed to
                                            --   begin
  , timed_post_process :: FilePath -> IO () -- ^ processing function called asynchronously after a file is added to the rotation
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

-- | Prefix file name with formatted time
prefixTime :: FormattedTime -> FilePath -> FilePath
prefixTime time path = dropFileName path </> unpack time ++ "-" ++ takeFileName path
