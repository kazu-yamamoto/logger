module FastLoggerSpec where

import Control.Concurrent
import Control.Exception (bracket)
import qualified Data.ByteString.Char8 as BS
import Data.IORef
import System.IO
import System.Log.FastLogger
import Test.Hspec
import Test.QuickCheck
import Test.QuickCheck.Monadic

spec :: Spec
spec = describe "hPutLogStr" $ do
    it "writes a log message" $ property prop_write
    it "is safe for a large message" safeForLarge

prop_write :: Property
prop_write = mapSize (*100) write

nullLogger :: IO Logger
nullLogger = openFile "/dev/null" WriteMode >>= mkLogger True

write :: String -> Property
write xs = monadicIO $ do
    res <- run $ bracket nullLogger rmLogger $ \lgr -> do
        ref <- newIORef False
        tid <- forkIO $ tryWrite ref lgr msg
        threadDelay 1000
        killThread tid
        readIORef ref
    assert res
  where
    msg = [LB (BS.pack xs)]

tryWrite :: IORef Bool -> Logger -> [LogStr] -> IO ()
tryWrite ref lgr msg = do
    loggerPutStr lgr msg
    writeIORef ref True

safeForLarge :: IO ()
safeForLarge = bracket nullLogger rmLogger $ \lgr ->
    loggerPutStr lgr [LS $ replicate 200000 'x']
