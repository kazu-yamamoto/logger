{-# LANGUAGE RecordWildCards #-}

module System.Log.FastLogger.SingleLogger (
    SingleLogger(..)
  , newSingleLogger
  ) where

import Control.Concurrent (forkIO, newEmptyMVar, MVar, takeMVar, putMVar)
import Control.Concurrent.STM

import System.Log.FastLogger.FileIO
import System.Log.FastLogger.IO
import System.Log.FastLogger.Imports
import System.Log.FastLogger.LogStr
import System.Log.FastLogger.Write

----------------------------------------------------------------

data SingleLogger = SingleLogger {
    slgrRef     :: IORef (LogStr
                         ,[LogStr])-- writer queue
  , slgrKill    :: IO ()
  , slgrWakeup  :: IO ()
  , slgrBuffer  :: Buffer
  , slgrBufSize :: BufSize
  , slgrFdRef   :: IORef FD
  }

instance Loggers SingleLogger where
    stopLoggers = System.Log.FastLogger.SingleLogger.stopLoggers
    pushLog     = System.Log.FastLogger.SingleLogger.pushLog
    flushAllLog = System.Log.FastLogger.SingleLogger.flushAllLog

----------------------------------------------------------------

writer :: BufSize -> Buffer -> IORef FD -> TVar Int -> IORef (LogStr, [LogStr]) -> MVar () -> IO ()
writer bufsize buf fdref tvar ref mvar = loop (0 :: Int)
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
      | len <  0       = return False
      | len <= bufsize = writeLogStr buf fdref msg >> go msgs
      | otherwise      = writeBigLogStr  fdref msg >> go msgs

----------------------------------------------------------------

newSingleLogger :: BufSize -> IORef FD -> IO SingleLogger
newSingleLogger bufsize fdref = do
    tvar <- newTVarIO 0
    ref <- newIORef (mempty,[])
    mvar <- newEmptyMVar
    buf <- getBuffer bufsize
    _ <- forkIO $ writer bufsize buf fdref tvar ref mvar
    let kill = do
            let fin = LogStr (-1) mempty
            atomicModifyIORef' ref (\(old,q) -> ((mempty,fin:old:q),()))
            takeMVar mvar
        wakeup = atomically $ modifyTVar' tvar (+ 1)
    return $ SingleLogger {
        slgrRef     = ref
      , slgrKill    = kill
      , slgrWakeup  = wakeup
      , slgrBuffer  = buf
      , slgrBufSize = bufsize
      , slgrFdRef   = fdref
      }

----------------------------------------------------------------

pushLog :: SingleLogger -> LogStr -> IO ()
pushLog SingleLogger{..} nlogmsg@(LogStr nlen _)
  | nlen > slgrBufSize = do
        atomicModifyIORef' slgrRef (\(old,q) -> ((mempty,nlogmsg:old:q),()))
        slgrWakeup
  | otherwise = do
        wake <- atomicModifyIORef' slgrRef checkBuf
        when wake slgrWakeup
  where
    checkBuf (ologmsg@(LogStr olen _),q)
      | slgrBufSize < olen + nlen = ((nlogmsg, nlogmsg:q), True)
      | otherwise                 = ((ologmsg <> nlogmsg, q), False)

flushAllLog :: SingleLogger -> IO ()
flushAllLog SingleLogger{..} = do
    atomicModifyIORef' slgrRef (\(old,q) -> ((mempty,old:q),()))
    slgrWakeup

stopLoggers :: SingleLogger -> IO ()
stopLoggers SingleLogger{..} = do
    slgrKill
    freeBuffer slgrBuffer
