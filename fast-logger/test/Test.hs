{-# LANGUAGE CPP, TemplateHaskell #-}

module Main where

import Test.Framework.Providers.QuickCheck2
import Test.Framework.TH.Prime
import Test.QuickCheck
import Test.QuickCheck.Monadic

import Control.Concurrent
import qualified Data.ByteString.Char8 as BS
import Data.IORef
import System.IO
import qualified System.Log.FastLogger as FastLogger

main :: IO ()
main = $(defaultMainGenerator)

prop_write :: Property
prop_write = mapSize (*100) write

write :: String -> Property
write xs = monadicIO $ do
    res <- run $ do
        ref <- newIORef False
        let msg = [FastLogger.LB (BS.pack xs)]
        hdl <- openFile "/dev/null" WriteMode
        tid <- forkIO (tryWrite ref hdl msg)
        threadDelay 1000
        hClose hdl
        killThread tid
        readIORef ref
    assert res
    
tryWrite :: IORef Bool -> Handle -> [FastLogger.LogStr] -> IO ()
tryWrite ref hdl msg = do
    FastLogger.hPutLogStr hdl msg
    writeIORef ref True
    
