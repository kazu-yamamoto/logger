{-# LANGUAGE CPP #-}

module System.Log.FastLogger.FileIO where

import Foreign.Ptr (Ptr)
import Data.Word (Word8)

#ifdef mingw32_HOST_OS
import System.Win32.Types (HANDLE, UINT)
import System.Win32.File
import Graphics.Win32.Misc (getStdHandle, sTD_OUTPUT_HANDLE, sTD_ERROR_HANDLE)
import Data.Bits ((.|.))

type FD = HANDLE
#if !MIN_VERSION_Win32(2,4,0)
-- This flag is not defined in System.Win32.File
fILE_APPEND_DATA :: UINT
fILE_APPEND_DATA = 0x0004
#endif

closeFD :: FD -> IO ()
closeFD = closeHandle

openFileFD :: FilePath -> IO FD
openFileFD f = createFile f
                         fILE_APPEND_DATA
                         (fILE_SHARE_READ .|. fILE_SHARE_DELETE)
                         Nothing
                         oPEN_ALWAYS
                         fILE_ATTRIBUTE_NORMAL
                         Nothing

getStderrFD :: IO FD
getStderrFD = getStdHandle sTD_ERROR_HANDLE

getStdoutFD :: IO FD
getStdoutFD = getStdHandle sTD_OUTPUT_HANDLE

writeRawBufferPtr2FD :: FD -> Ptr Word8 -> Int -> IO Int
writeRawBufferPtr2FD h bf len = fromIntegral `fmap` win32_WriteFile h bf (fromIntegral len) Nothing

#else

import GHC.IO.Device (close)
import qualified GHC.IO.FD as POSIX (FD(..))
import GHC.IO.FD (openFile, stderr, stdout,  writeRawBufferPtr)
import GHC.IO.IOMode (IOMode(..))

type FD = POSIX.FD

closeFD :: FD -> IO ()
closeFD = close

openFileFD :: FilePath -> IO FD
openFileFD f = fst `fmap` openFile f AppendMode False

getStderrFD :: IO FD
getStderrFD = return stderr

getStdoutFD :: IO FD
getStdoutFD = return stdout

writeRawBufferPtr2FD :: FD -> Ptr Word8 -> Int -> IO Int
writeRawBufferPtr2FD fd bf len = fromIntegral `fmap` writeRawBufferPtr "write" fd bf 0 (fromIntegral len)

#endif
