{-# LANGUAGE CPP #-}
{-# LANGUAGE Safe #-}

module System.Log.FastLogger.IORef (
       IORef
     , newIORef
     , readIORef
     , atomicModifyIORef'
     , writeIORef
     ) where

import Data.IORef

#if !MIN_VERSION_base(4, 6, 0)
atomicModifyIORef' :: IORef a -> (a -> (a,b)) -> IO b
atomicModifyIORef' ref f = do
    b <- atomicModifyIORef ref
            (\x -> let (a, b) = f x
                    in (a, a `seq` b))
    b `seq` return b
#endif
