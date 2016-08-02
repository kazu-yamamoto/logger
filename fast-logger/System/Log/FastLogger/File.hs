{-# LANGUAGE Safe #-}

module System.Log.FastLogger.File where

import Control.Monad (unless, when)
import System.Directory (doesFileExist, doesDirectoryExist, getPermissions, writable, renameFile)
import System.FilePath (takeDirectory)

-- | The spec for logging files
data FileLogSpec = FileLogSpec {
    log_file :: FilePath
  , log_file_size :: Integer -- ^ Max log file size (in bytes) before requiring rotation.
  , log_backup_number :: Int -- ^ Max number of rotated log files to keep around before overwriting the oldest one.
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
