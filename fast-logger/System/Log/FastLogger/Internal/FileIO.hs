module System.Log.FastLogger.Internal.FileIO where

import Foreign.Ptr (Ptr)
import GHC.IO.Device (close)
import GHC.IO.FD (openFile, stderr, stdout,  writeRawBufferPtr)
import qualified GHC.IO.FD as POSIX (FD(..))
import GHC.IO.IOMode (IOMode(..))

import System.Log.FastLogger.Internal.Imports

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
    fromIntegral <$> writeRawBufferPtr "write" fd bf 0 (fromIntegral len)
