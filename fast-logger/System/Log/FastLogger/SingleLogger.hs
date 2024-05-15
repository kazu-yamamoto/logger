{-# LANGUAGE RecordWildCards #-}

module System.Log.FastLogger.SingleLogger (
    SingleLogger,
    newSingleLogger,
) where

import Control.Concurrent (MVar, forkIO, newEmptyMVar, putMVar, takeMVar)
import Control.Concurrent.STM

import System.Log.FastLogger.FileIO
import System.Log.FastLogger.IO
import System.Log.FastLogger.Imports
import System.Log.FastLogger.LogStr
import System.Log.FastLogger.Write

----------------------------------------------------------------

data Ent = F (MVar ()) Bool | L LogStr
type Q = [Ent] -- writer queue

-- | A non-scale but time-ordered logger.
data SingleLogger = SingleLogger
    { slgrRef :: IORef (LogStr, Q)
    , slgrFlush :: Bool -> IO () -- teminate if False
    , slgrWakeup :: IO ()
    , slgrBuffer :: Buffer
    , slgrBufSize :: BufSize
    , slgrFdRef :: IORef FD
    }

instance Loggers SingleLogger where
    stopLoggers = System.Log.FastLogger.SingleLogger.stopLoggers
    pushLog = System.Log.FastLogger.SingleLogger.pushLog
    flushAllLog = System.Log.FastLogger.SingleLogger.flushAllLog

----------------------------------------------------------------

writer
    :: BufSize
    -> Buffer
    -> IORef FD
    -> TVar Int
    -> IORef (LogStr, Q)
    -> IO ()
writer bufsize buf fdref tvar ref = loop (0 :: Int)
  where
    loop cnt = do
        cnt' <- atomically $ do
            n <- readTVar tvar
            check (n /= cnt)
            return n
        msgs <- reverse <$> atomicModifyIORef' ref (\(msg, q) -> ((msg, []), q))
        cont <- go msgs
        when cont $ loop cnt'
    go [] = return True
    go (F mvar cont : msgs) = do
        putMVar mvar ()
        if cont then go msgs else return False
    go (L msg@(LogStr len _) : msgs)
        | len <= bufsize = writeLogStr buf fdref msg >> go msgs
        | otherwise = writeBigLogStr fdref msg >> go msgs

----------------------------------------------------------------

-- | Creating `SingleLogger`.
newSingleLogger :: BufSize -> IORef FD -> IO SingleLogger
newSingleLogger bufsize fdref = do
    tvar <- newTVarIO 0
    ref <- newIORef (mempty, [])
    buf <- getBuffer bufsize
    _ <- forkIO $ writer bufsize buf fdref tvar ref
    let wakeup = atomically $ modifyTVar' tvar (+ 1)
        flush cont = do
            mvar <- newEmptyMVar
            let fin = F mvar cont
            atomicModifyIORef' ref (\(old, q) -> ((mempty, fin : L old : q), ()))
            wakeup
            takeMVar mvar
    return $
        SingleLogger
            { slgrRef = ref
            , slgrFlush = flush
            , slgrWakeup = wakeup
            , slgrBuffer = buf
            , slgrBufSize = bufsize
            , slgrFdRef = fdref
            }

----------------------------------------------------------------

pushLog :: SingleLogger -> LogStr -> IO ()
pushLog SingleLogger{..} nlogmsg@(LogStr nlen _)
    | nlen > slgrBufSize = do
        atomicModifyIORef' slgrRef (\(old, q) -> ((mempty, L nlogmsg : L old : q), ()))
        slgrWakeup
    | otherwise = do
        wake <- atomicModifyIORef' slgrRef checkBuf
        when wake slgrWakeup
  where
    checkBuf (ologmsg@(LogStr olen _), q)
        | slgrBufSize < olen + nlen = ((nlogmsg, L ologmsg : q), True)
        | otherwise = ((ologmsg <> nlogmsg, q), False)

flushAllLog :: SingleLogger -> IO ()
flushAllLog SingleLogger{..} = do
    atomicModifyIORef' slgrRef (\(old, q) -> ((mempty, L old : q), ()))
    slgrFlush True

stopLoggers :: SingleLogger -> IO ()
stopLoggers SingleLogger{..} = do
    slgrFlush False
    freeBuffer slgrBuffer
