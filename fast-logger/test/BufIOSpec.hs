{-# LANGUAGE OverloadedStrings #-}

module BufIOSpec where

import qualified Data.ByteString.Char8 as BS
import Data.IORef
import System.Log.FastLogger
import System.Log.FastLogger.Internal
import Test.Hspec

spec :: Spec
spec = describe "toBufIOWith (#81)" $ do
    it "does not write an empty builder" $
        lengths mempty `shouldReturn` []
    it "does not write empty steps around a big chunk" $
        lengths (toLogStr (BS.replicate 100000 'x')) `shouldReturn` [100000]
    it "writes a small message once" $
        lengths "hello\n" `shouldReturn` [6]

-- The lengths passed to the writing action.
lengths :: LogStr -> IO [Int]
lengths (LogStr len builder) = do
    ref <- newIORef []
    buf <- getBuffer (max 1 len)
    toBufIOWith buf len (\_ n -> modifyIORef ref (n :)) builder
    freeBuffer buf
    reverse <$> readIORef ref
