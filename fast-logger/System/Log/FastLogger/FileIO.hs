{-# LANGUAGE CPP #-}

-- | Where the bytes actually go.
--
-- The rest of fast-logger fills its own buffer and hands it here as a raw
-- pointer, so this is deliberately one layer below 'Handle': the point of
-- 'writeRawBufferPtr2FD' is that nothing buffers the bytes a second time.
--
-- Windows has two I\/O subsystems and chooses between them at run time, from
-- the @--io-manager=@ RTS flag, so on Windows an 'FD' is one of two things.
-- See the note on 'openFileFD'.
module System.Log.FastLogger.FileIO (
    FD,
    closeFD,
    openFileFD,
    getStderrFD,
    getStdoutFD,
    writeRawBufferPtr2FD,
    invalidFD,
    isFDValid,
) where

import Foreign.Ptr (Ptr)
import GHC.IO.Device (close)
import GHC.IO.FD (openFile, stderr, stdout, writeRawBufferPtr)
import qualified GHC.IO.FD as POSIX (FD (..))
import GHC.IO.IOMode (IOMode (..))

import System.Log.FastLogger.Imports

#if defined(mingw32_HOST_OS)
import GHC.IO.SubSystem (isWindowsNativeIO)
import qualified System.IO as SIO
#endif

#if defined(mingw32_HOST_OS)

-- | A file descriptor, or on Windows under the native I\/O manager a
--   'SIO.Handle' standing in for one.
data FD
    = PosixFD !POSIX.FD
    | -- | Under the native I\/O manager.  The 'SIO.Handle' is left with
      -- whatever buffering it came with and flushed after every write, so
      -- the bytes are no more buffered than they were before.
      NativeFD !SIO.Handle
    | InvalidFD

-- | Opening a log file for appending.
--
-- Under the POSIX subsystem this is a file descriptor, as it always was.
--
-- Under the native subsystem it cannot be.  There a 'POSIX.FD' is a C
-- runtime descriptor, and the I\/O manager works in terms of Windows
-- handles registered with a completion port; the two cannot be mixed, and
-- @base@ says so by replacing every method of @IODevice FD@ and @RawIO FD@
-- with an @error@ when that subsystem is in force.  Opening and the raw
-- write happen to be plain functions and so still work, which is the trap:
-- a log file could be written and never closed, and it stayed locked for
-- the life of the process.
--
-- 'SIO.openFile' gives a handle the running subsystem owns, whichever it
-- is.  Appending is its business too, which matters because the native
-- subsystem writes at an offset given per call and has no @O_APPEND@ of
-- its own.
openFileFD :: FilePath -> IO FD
openFileFD f
    | isWindowsNativeIO = NativeFD <$> SIO.openFile f SIO.AppendMode
    | otherwise = PosixFD . fst <$> openFile f AppendMode False

-- | The standard streams are handed to us, not opened by us, and under the
--   native subsystem the descriptor numbered 1 is not what the process is
--   actually writing through: bytes put there were accepted and never
--   appeared.
getStdoutFD :: IO FD
getStdoutFD
    | isWindowsNativeIO = return $ NativeFD SIO.stdout
    | otherwise = return $ PosixFD stdout

getStderrFD :: IO FD
getStderrFD
    | isWindowsNativeIO = return $ NativeFD SIO.stderr
    | otherwise = return $ PosixFD stderr

-- | Only ever called for a log file, never for a standard stream.
closeFD :: FD -> IO ()
closeFD (PosixFD fd) = close fd
closeFD (NativeFD h) = SIO.hClose h
closeFD InvalidFD = return ()

writeRawBufferPtr2FD :: IORef FD -> Ptr Word8 -> Int -> IO Int
writeRawBufferPtr2FD fdref bf len = do
    fd <- readIORef fdref
    case fd of
        PosixFD fd'
            | POSIX.fdFD fd' /= -1 ->
                fromIntegral <$> writeRawBufferPtr "write" fd' bf 0 (fromIntegral len)
        -- 'SIO.hPutBuf' writes all of it or raises; there is no short write
        -- to report back.
        NativeFD h -> do
            SIO.hPutBuf h bf len
            SIO.hFlush h
            return len
        _ -> return (-1)

invalidFD :: FD
invalidFD = InvalidFD

isFDValid :: FD -> Bool
isFDValid (PosixFD fd) = POSIX.fdFD fd /= -1
isFDValid (NativeFD _) = True
isFDValid InvalidFD = False

#else

type FD = POSIX.FD

closeFD :: FD -> IO ()
closeFD = close

openFileFD :: FilePath -> IO FD
openFileFD f = fst <$> openFile f AppendMode False

getStderrFD :: IO FD
getStderrFD = return stderr

getStdoutFD :: IO FD
getStdoutFD = return stdout

writeRawBufferPtr2FD :: IORef FD -> Ptr Word8 -> Int -> IO Int
writeRawBufferPtr2FD fdref bf len = do
    fd <- readIORef fdref
    if isFDValid fd
        then
            fromIntegral <$> writeRawBufferPtr "write" fd bf 0 (fromIntegral len)
        else
            return (-1)

invalidFD :: POSIX.FD
invalidFD = stdout{POSIX.fdFD = -1}

isFDValid :: POSIX.FD -> Bool
isFDValid fd = POSIX.fdFD fd /= -1

#endif
