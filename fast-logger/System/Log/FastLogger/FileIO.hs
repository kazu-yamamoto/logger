module System.Log.FastLogger.FileIO where

import Data.Word (Word8)
import Foreign.Ptr (Ptr)
import GHC.IO.Device (close)
import GHC.IO.FD (openFile, stderr, stdout,  writeRawBufferPtr)
import qualified GHC.IO.FD as POSIX (FD(..))
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
