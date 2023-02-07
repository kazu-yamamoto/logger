{-# LANGUAGE RecordWildCards #-}

module System.Log.FastLogger.SingleLogger (
    SingleLogger(..)
  , newSingleLogger
  ) where

import Control.Concurrent (forkIO, newEmptyMVar, MVar, takeMVar, putMVar)
import Control.Concurrent.STM

import System.Log.FastLogger.Imports
import System.Log.FastLogger.LogStr
import System.Log.FastLogger.Write

----------------------------------------------------------------

data SingleLogger = SingleLogger {
    slgrRef  :: IORef (LogStr
                      ,[LogStr] -- writer queue
                      )
  , slgrKill :: IO ()
  , slgrWakeup :: IO ()
  }

instance Loggers SingleLogger where
    stopLoggers = System.Log.FastLogger.SingleLogger.flushAllLog
    pushAllLog  = System.Log.FastLogger.SingleLogger.pushAllLog
    flushAllLog = System.Log.FastLogger.SingleLogger.shutdown

----------------------------------------------------------------

writer :: BufFD -> TVar Int -> IORef (LogStr, [LogStr]) -> MVar () -> IO ()
writer buffd@BufFD{..} tvar ref mvar = loop (0 :: Int)
  where
    loop cnt = do
        cnt' <- atomically $ do
            n <- readTVar tvar
            check (n /= cnt)
            return n
        msgs <- reverse <$> atomicModifyIORef' ref (\(msg,q) -> ((msg,[]),q))
        cont <- go msgs
        if cont then
            loop cnt'
          else
            putMVar mvar ()
    go [] = return True
    go (msg@(LogStr len _):msgs)
      | len <  0            = return False
      | len <= buffdBufSize = writeLogStr buffd msg    >> go msgs
      | otherwise           = writeBigLogStr buffd msg >> go msgs

----------------------------------------------------------------

newSingleLogger :: BufFD -> IO SingleLogger
newSingleLogger buffd = do
    tvar <- newTVarIO 0
    ref <- newIORef (mempty,[])
    mvar <- newEmptyMVar
    _ <- forkIO $ writer buffd tvar ref mvar
    let kill = do
            let fin = LogStr (-1) mempty
            atomicModifyIORef' ref (\(old,q) -> ((mempty,fin:old:q),()))
            takeMVar mvar
        wakeup = atomically $ modifyTVar' tvar (+ 1)
    return $ SingleLogger {
        slgrRef    = ref
      , slgrKill   = kill
      , slgrWakeup = wakeup
      }

----------------------------------------------------------------

pushAllLog :: SingleLogger -> BufFD -> LogStr -> IO ()
pushAllLog SingleLogger{..} BufFD{..} nlogmsg@(LogStr nlen _)
  | nlen > buffdBufSize = do
        atomicModifyIORef' slgrRef (\(old,q) -> ((mempty,nlogmsg:old:q),()))
        slgrWakeup
  | otherwise = do
        wake <- atomicModifyIORef' slgrRef checkBuf
        when wake $ slgrWakeup
  where
    checkBuf (ologmsg@(LogStr olen _),q)
      | buffdBufSize < olen + nlen = ((nlogmsg, nlogmsg:q), True)
      | otherwise                  = ((ologmsg <> nlogmsg, q), False)

flushAllLog :: SingleLogger -> BufFD -> IO ()
flushAllLog SingleLogger{..} _ = do
    atomicModifyIORef' slgrRef (\(old,q) -> ((mempty,old:q),()))
    slgrWakeup

----------------------------------------------------------------

shutdown :: SingleLogger -> BufFD -> IO ()
shutdown SingleLogger{..} _ = slgrKill
