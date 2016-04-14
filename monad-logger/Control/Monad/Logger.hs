{-# LANGUAGE CPP #-}
{-# LANGUAGE DefaultSignatures #-}
#if WITH_TEMPLATE_HASKELL
{-# LANGUAGE TemplateHaskell #-}
#endif
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE Trustworthy #-}
-- |  This module provides the facilities needed for a decoupled logging system.
--
-- The 'MonadLogger' class is implemented by monads that give access to a
-- logging facility.  If you're defining a custom monad, then you may define an
-- instance of 'MonadLogger' that routes the log messages to the appropriate
-- place (e.g., that's what @yesod-core@'s @GHandler@ does).  Otherwise, you
-- may use the 'LoggingT' monad included in this module (see
-- 'runStderrLoggingT'). To simply discard log message, use 'NoLoggingT'.
--
-- As a user of the logging facility, we provide you some convenient Template
-- Haskell splices that use the 'MonadLogger' class.  They will record their
-- source file and position, which is very helpful when debugging.  See
-- 'logDebug' for more information.
module Control.Monad.Logger
    ( -- * MonadLogger
      MonadLogger(..)
    , MonadLoggerIO (..)
    , LogLevel(..)
    , LogSource
    -- * Re-export from fast-logger
    , module System.Log.FastLogger
    , module System.Log.FastLogger.Date
    , module System.Log.FastLogger.File
    -- * Helper transformer
    , LoggingT (..)
    , runStderrLoggingT
    , runStdoutLoggingT
    , runFastLoggerLoggingT
    , runTimedFastLoggerLoggingT
    , runChanLoggingT
    , unChanLoggingT
    , withChannelLogger
    , filterLogger
    , NoLoggingT (..)
#if WITH_TEMPLATE_HASKELL
    -- * TH logging
    , logDebug
    , logInfo
    , logWarn
    , logError
    , logOther
    -- * TH logging of showable values
    , logDebugSH
    , logInfoSH
    , logWarnSH
    , logErrorSH
    , logOtherSH
    -- * TH logging with source
    , logDebugS
    , logInfoS
    , logWarnS
    , logErrorS
    , logOtherS
    -- * TH util
    , liftLoc
#endif
    -- * Non-TH logging
    , logDebugN
    , logInfoN
    , logWarnN
    , logErrorN
    , logOtherN
    -- * Non-TH logging with source
    , logDebugNS
    , logInfoNS
    , logWarnNS
    , logErrorNS
    , logOtherNS

    -- * utilities for defining your own loggers
    , defaultLogStr
    , defaultTimedLogStr
    , Loc (..)
    , fileLocStr
    ) where

#if WITH_TEMPLATE_HASKELL
import Language.Haskell.TH.Syntax (Lift (lift), Q, Exp, Loc (..), qLocation)
#endif

import Data.Monoid (Monoid)

import Control.Applicative (Applicative (..))
import Control.Concurrent.Chan (Chan(),writeChan,readChan)
import Control.Concurrent.STM
import Control.Concurrent.STM.TBChan
import Control.Exception.Lifted (onException)
import Control.Monad (liftM, ap, when, void, forever)
import Control.Monad.Base (MonadBase (liftBase))
import Control.Monad.Loops (untilM)
import Control.Monad.Trans.Control (MonadBaseControl (..), MonadTransControl (..))
import qualified Control.Monad.Trans.Class as Trans

import Control.Monad.IO.Class (MonadIO (liftIO))
import Control.Monad.Trans.Resource (MonadResource (liftResourceT), MonadThrow, monadThrow)
#if MIN_VERSION_resourcet(1,1,0)
import Control.Monad.Trans.Resource (throwM)
import Control.Monad.Catch (MonadCatch (..)
#if MIN_VERSION_exceptions(0,6,0)
    , MonadMask (..)
#endif
    )
#endif

import Control.Monad.Trans.Identity ( IdentityT)
import Control.Monad.Trans.List     ( ListT    )
import Control.Monad.Trans.Maybe    ( MaybeT   )
import Control.Monad.Trans.Error    ( ErrorT, Error)
import Control.Monad.Trans.Except   ( ExceptT  )

import Control.Monad.Trans.Reader   ( ReaderT  )
import Control.Monad.Trans.Cont     ( ContT  )
import Control.Monad.Trans.State    ( StateT   )
import Control.Monad.Trans.Writer   ( WriterT  )
import Control.Monad.Trans.RWS      ( RWST     )
import Control.Monad.Trans.Resource ( ResourceT)
import Data.Conduit.Internal        ( Pipe, ConduitM )

import qualified Control.Monad.Trans.RWS.Strict    as Strict ( RWST   )
import qualified Control.Monad.Trans.State.Strict  as Strict ( StateT )
import qualified Control.Monad.Trans.Writer.Strict as Strict ( WriterT )

import Data.Text (Text, pack, unpack)
import qualified Data.Text as T
import qualified Data.ByteString.Char8 as S8

import Data.Monoid (mappend, mempty)
import System.Log.FastLogger
import System.Log.FastLogger.Date
import System.Log.FastLogger.File
import System.IO (Handle, stdout, stderr)

import Control.Monad.Cont.Class   ( MonadCont (..) )
import Control.Monad.Error.Class  ( MonadError (..) )
import Control.Monad.RWS.Class    ( MonadRWS )
import Control.Monad.Reader.Class ( MonadReader (..) )
import Control.Monad.State.Class  ( MonadState (..) )
import Control.Monad.Writer.Class ( MonadWriter (..) )

import Prelude hiding (catch)

#if MIN_VERSION_fast_logger(2, 1, 0)
-- Using System.Log.FastLogger
#elif MIN_VERSION_bytestring(0, 10, 2)
import qualified Data.ByteString.Lazy as L
import Data.ByteString.Builder (toLazyByteString)
#else
import Blaze.ByteString.Builder (toByteString)
#endif

#if MIN_VERSION_conduit_extra(1,1,0)
import Data.Conduit.Lazy (MonadActive, monadActive)
#endif

data LogLevel = LevelDebug | LevelInfo | LevelWarn | LevelError | LevelOther Text
    deriving (Eq, Prelude.Show, Prelude.Read, Ord)

type LogSource = Text

#if WITH_TEMPLATE_HASKELL

instance Lift LogLevel where
    lift LevelDebug = [|LevelDebug|]
    lift LevelInfo  = [|LevelInfo|]
    lift LevelWarn  = [|LevelWarn|]
    lift LevelError = [|LevelError|]
    lift (LevelOther x) = [|LevelOther $ pack $(lift $ unpack x)|]

#else

data Loc
  = Loc { loc_filename :: String
    , loc_package  :: String
    , loc_module   :: String
    , loc_start    :: CharPos
    , loc_end      :: CharPos }
type CharPos = (Int, Int)

#endif

-- | A @Monad@ which has the ability to log messages in some manner.
class Monad m => MonadLogger m where
    monadLoggerLog :: ToLogStr msg => Loc -> LogSource -> LogLevel -> msg -> m ()
    default monadLoggerLog :: (Trans.MonadTrans t, MonadLogger (t m), ToLogStr msg)
                           => Loc -> LogSource -> LogLevel -> msg -> t m ()
    monadLoggerLog loc src lvl msg = Trans.lift $ monadLoggerLog loc src lvl msg

-- | An extension of @MonadLogger@ for the common case where the logging action
-- is a simple @IO@ action. The advantage of using this typeclass is that the
-- logging function itself can be extracted as a first-class value, which can
-- make it easier to manipulate monad transformer stacks, as an example.
--
-- Since 0.3.10
class (MonadLogger m, MonadIO m) => MonadLoggerIO m where
    -- | Request the logging function itself.
    --
    -- Since 0.3.10
    askLoggerIO :: m (Loc -> LogSource -> LogLevel -> LogStr -> IO ())
    default askLoggerIO :: (Trans.MonadTrans t, MonadLogger (t m), MonadIO (t m))
                        => t m (Loc -> LogSource -> LogLevel -> LogStr -> IO ())
    askLoggerIO = Trans.lift askLoggerIO


{-
instance MonadLogger IO          where monadLoggerLog _ _ _ = return ()
instance MonadLogger Identity    where monadLoggerLog _ _ _ = return ()
instance MonadLogger (ST s)      where monadLoggerLog _ _ _ = return ()
instance MonadLogger (Lazy.ST s) where monadLoggerLog _ _ _ = return ()
-}

#define DEF monadLoggerLog a b c d = Trans.lift $ monadLoggerLog a b c d
instance MonadLogger m => MonadLogger (IdentityT m) where DEF
instance MonadLogger m => MonadLogger (ListT m) where DEF
instance MonadLogger m => MonadLogger (MaybeT m) where DEF
instance (MonadLogger m, Error e) => MonadLogger (ErrorT e m) where DEF
instance MonadLogger m => MonadLogger (ExceptT e m) where DEF
instance MonadLogger m => MonadLogger (ReaderT r m) where DEF
instance MonadLogger m => MonadLogger (ContT r m) where DEF
instance MonadLogger m => MonadLogger (StateT s m) where DEF
instance (MonadLogger m, Monoid w) => MonadLogger (WriterT w m) where DEF
instance (MonadLogger m, Monoid w) => MonadLogger (RWST r w s m) where DEF
instance MonadLogger m => MonadLogger (ResourceT m) where DEF
instance MonadLogger m => MonadLogger (Pipe l i o u m) where DEF
instance MonadLogger m => MonadLogger (ConduitM i o m) where DEF
instance MonadLogger m => MonadLogger (Strict.StateT s m) where DEF
instance (MonadLogger m, Monoid w) => MonadLogger (Strict.WriterT w m) where DEF
instance (MonadLogger m, Monoid w) => MonadLogger (Strict.RWST r w s m) where DEF
#undef DEF

instance MonadLoggerIO m => MonadLoggerIO (IdentityT m)
instance MonadLoggerIO m => MonadLoggerIO (ListT m)
instance MonadLoggerIO m => MonadLoggerIO (MaybeT m)
instance (MonadLoggerIO m, Error e) => MonadLoggerIO (ErrorT e m)
instance MonadLoggerIO m => MonadLoggerIO (ExceptT e m)
instance MonadLoggerIO m => MonadLoggerIO (ReaderT r m)
instance MonadLoggerIO m => MonadLoggerIO (ContT r m)
instance MonadLoggerIO m => MonadLoggerIO (StateT s m)
instance (MonadLoggerIO m, Monoid w) => MonadLoggerIO (WriterT w m)
instance (MonadLoggerIO m, Monoid w) => MonadLoggerIO (RWST r w s m)
instance MonadLoggerIO m => MonadLoggerIO (ResourceT m)
instance MonadLoggerIO m => MonadLoggerIO (Pipe l i o u m)
instance MonadLoggerIO m => MonadLoggerIO (ConduitM i o m)
instance MonadLoggerIO m => MonadLoggerIO (Strict.StateT s m)
instance (MonadLoggerIO m, Monoid w) => MonadLoggerIO (Strict.WriterT w m)
instance (MonadLoggerIO m, Monoid w) => MonadLoggerIO (Strict.RWST r w s m)

#if WITH_TEMPLATE_HASKELL
logTH :: LogLevel -> Q Exp
logTH level =
    [|monadLoggerLog $(qLocation >>= liftLoc) (pack "") $(lift level)
     . (id :: Text -> Text)|]

-- | Generates a function that takes a 'LogLevel' and a 'Show a => a'.
--
-- @since 0.3.18
logTHShow :: LogLevel -> Q Exp
logTHShow level =
    [|monadLoggerLog $(qLocation >>= liftLoc) (pack "") $(lift level)
      . ((pack . show) :: Show a => a -> Text)|]

-- | Generates a function that takes a 'Text' and logs a 'LevelDebug' message. Usage:
--
-- > $(logDebug) "This is a debug log message"
logDebug :: Q Exp
logDebug = logTH LevelDebug

-- | See 'logDebug'
logInfo :: Q Exp
logInfo = logTH LevelInfo
-- | See 'logDebug'
logWarn :: Q Exp
logWarn = logTH LevelWarn
-- | See 'logDebug'
logError :: Q Exp
logError = logTH LevelError

-- | Generates a function that takes a 'Text' and logs a 'LevelOther' message. Usage:
--
-- > $(logOther "My new level") "This is a log message"
logOther :: Text -> Q Exp
logOther = logTH . LevelOther


-- | Generates a function that takes a 'Show a => a' and logs a 'LevelDebug' message. Usage:
--
-- > $(logDebugSH) (Just "This is a debug log message")
--
-- @since 0.3.18
logDebugSH :: Q Exp
logDebugSH = logTHShow LevelDebug

-- | See 'logDebugSH'
logInfoSH :: Q Exp
logInfoSH = logTHShow LevelInfo
-- | See 'logDebugSH'
logWarnSH :: Q Exp
logWarnSH = logTHShow LevelWarn
-- | See 'logDebugSH'
logErrorSH :: Q Exp
logErrorSH = logTHShow LevelError

-- | Generates a function that takes a 'Show a => a' and logs a 'LevelOther' message. Usage:
--
-- > $(logOtherSH "My new level") "This is a log message"
logOtherSH :: Text -> Q Exp
logOtherSH = logTHShow . LevelOther

-- | Lift a location into an Exp.
--
-- Since 0.3.1
liftLoc :: Loc -> Q Exp
liftLoc (Loc a b c (d1, d2) (e1, e2)) = [|Loc
    $(lift a)
    $(lift b)
    $(lift c)
    ($(lift d1), $(lift d2))
    ($(lift e1), $(lift e2))
    |]

-- | Generates a function that takes a 'LogSource' and 'Text' and logs a 'LevelDebug' message. Usage:
--
-- > $logDebugS "SomeSource" "This is a debug log message"
logDebugS :: Q Exp
logDebugS = [|\a b -> monadLoggerLog $(qLocation >>= liftLoc) a LevelDebug (b :: Text)|]

-- | See 'logDebugS'
logInfoS :: Q Exp
logInfoS = [|\a b -> monadLoggerLog $(qLocation >>= liftLoc) a LevelInfo (b :: Text)|]
-- | See 'logDebugS'
logWarnS :: Q Exp
logWarnS = [|\a b -> monadLoggerLog $(qLocation >>= liftLoc) a LevelWarn (b :: Text)|]
-- | See 'logDebugS'
logErrorS :: Q Exp
logErrorS = [|\a b -> monadLoggerLog $(qLocation >>= liftLoc) a LevelError (b :: Text)|]

-- | Generates a function that takes a 'LogSource', a level name and a 'Text' and logs a 'LevelOther' message. Usage:
--
-- > $logOtherS "SomeSource" "My new level" "This is a log message"
logOtherS :: Q Exp
logOtherS = [|\src level msg -> monadLoggerLog $(qLocation >>= liftLoc) src (LevelOther level) (msg :: Text)|]
#endif

-- | Monad transformer that disables logging.
--
-- Since 0.2.4
newtype NoLoggingT m a = NoLoggingT { runNoLoggingT :: m a }

instance Monad m => Functor (NoLoggingT m) where
    fmap = liftM

instance Monad m => Applicative (NoLoggingT m) where
    pure = return
    (<*>) = ap

instance Monad m => Monad (NoLoggingT m) where
    return = NoLoggingT . return
    NoLoggingT ma >>= f = NoLoggingT $ ma >>= runNoLoggingT . f

instance MonadIO m => MonadIO (NoLoggingT m) where
    liftIO = Trans.lift . liftIO

#if MIN_VERSION_resourcet(1,1,0)
instance MonadThrow m => MonadThrow (NoLoggingT m) where
    throwM = Trans.lift . throwM

instance MonadCatch m => MonadCatch (NoLoggingT m) where
    catch (NoLoggingT m) c =
        NoLoggingT $ m `catch` \e -> runNoLoggingT (c e)
#if MIN_VERSION_exceptions(0,6,0)
instance MonadMask m => MonadMask (NoLoggingT m) where
#endif
    mask a = NoLoggingT $ mask $ \u -> runNoLoggingT (a $ q u)
      where q u (NoLoggingT b) = NoLoggingT $ u b
    uninterruptibleMask a =
        NoLoggingT $ uninterruptibleMask $ \u -> runNoLoggingT (a $ q u)
      where q u (NoLoggingT b) = NoLoggingT $ u b
#else
instance MonadThrow m => MonadThrow (NoLoggingT m) where
    monadThrow = Trans.lift . monadThrow
#endif

#if MIN_VERSION_conduit_extra(1,1,0)
instance MonadActive m => MonadActive (NoLoggingT m) where
    monadActive = Trans.lift monadActive
instance MonadActive m => MonadActive (LoggingT m) where
    monadActive = Trans.lift monadActive
#endif

instance MonadResource m => MonadResource (NoLoggingT m) where
    liftResourceT = Trans.lift . liftResourceT

instance MonadBase b m => MonadBase b (NoLoggingT m) where
    liftBase = Trans.lift . liftBase

instance Trans.MonadTrans NoLoggingT where
    lift = NoLoggingT

instance MonadTransControl NoLoggingT where
#if MIN_VERSION_monad_control(1,0,0)
    type StT NoLoggingT a = a
    liftWith f = NoLoggingT $ f runNoLoggingT
    restoreT = NoLoggingT
#else
    newtype StT NoLoggingT a = StIdent {unStIdent :: a}
    liftWith f = NoLoggingT $ f $ \(NoLoggingT t) -> liftM StIdent t
    restoreT = NoLoggingT . liftM unStIdent
#endif
    {-# INLINE liftWith #-}
    {-# INLINE restoreT #-}

instance MonadBaseControl b m => MonadBaseControl b (NoLoggingT m) where
#if MIN_VERSION_monad_control(1,0,0)
     type StM (NoLoggingT m) a = StM m a
     liftBaseWith f = NoLoggingT $
         liftBaseWith $ \runInBase ->
             f $ runInBase . runNoLoggingT
     restoreM = NoLoggingT . restoreM
#else
     newtype StM (NoLoggingT m) a = StMT' (StM m a)
     liftBaseWith f = NoLoggingT $
         liftBaseWith $ \runInBase ->
             f $ liftM StMT' . runInBase . (\(NoLoggingT r) -> r)
     restoreM (StMT' base) = NoLoggingT $ restoreM base
#endif

instance Monad m => MonadLogger (NoLoggingT m) where
    monadLoggerLog _ _ _ _ = return ()
instance MonadIO m => MonadLoggerIO (NoLoggingT m) where
    askLoggerIO = return $ \_ _ _ _ -> return ()

-- | Monad transformer that adds a new logging function.
--
-- Since 0.2.2
newtype LoggingT m a = LoggingT { runLoggingT :: (Loc -> LogSource -> LogLevel -> LogStr -> IO ()) -> m a }

instance Monad m => Functor (LoggingT m) where
    fmap = liftM

instance Monad m => Applicative (LoggingT m) where
    pure = return
    (<*>) = ap

instance Monad m => Monad (LoggingT m) where
    return = LoggingT . const . return
    LoggingT ma >>= f = LoggingT $ \r -> do
        a <- ma r
        let LoggingT f' = f a
        f' r

instance MonadIO m => MonadIO (LoggingT m) where
    liftIO = Trans.lift . liftIO

#if MIN_VERSION_resourcet(1,1,0)
instance MonadThrow m => MonadThrow (LoggingT m) where
    throwM = Trans.lift . throwM
instance MonadCatch m => MonadCatch (LoggingT m) where
  catch (LoggingT m) c =
      LoggingT $ \r -> m r `catch` \e -> runLoggingT (c e) r
#if MIN_VERSION_exceptions(0,6,0)
instance MonadMask m => MonadMask (LoggingT m) where
#endif
  mask a = LoggingT $ \e -> mask $ \u -> runLoggingT (a $ q u) e
    where q u (LoggingT b) = LoggingT (u . b)
  uninterruptibleMask a =
    LoggingT $ \e -> uninterruptibleMask $ \u -> runLoggingT (a $ q u) e
      where q u (LoggingT b) = LoggingT (u . b)
#else
instance MonadThrow m => MonadThrow (LoggingT m) where
    monadThrow = Trans.lift . monadThrow
#endif

instance MonadResource m => MonadResource (LoggingT m) where
    liftResourceT = Trans.lift . liftResourceT

instance MonadBase b m => MonadBase b (LoggingT m) where
    liftBase = Trans.lift . liftBase

instance Trans.MonadTrans LoggingT where
    lift = LoggingT . const

instance MonadTransControl LoggingT where
#if MIN_VERSION_monad_control(1,0,0)
    type StT LoggingT a = a
    liftWith f = LoggingT $ \r -> f $ \(LoggingT t) -> t r
    restoreT = LoggingT . const
#else
    newtype StT LoggingT a = StReader {unStReader :: a}
    liftWith f = LoggingT $ \r -> f $ \(LoggingT t) -> liftM StReader $ t r
    restoreT = LoggingT . const . liftM unStReader
#endif
    {-# INLINE liftWith #-}
    {-# INLINE restoreT #-}

instance MonadBaseControl b m => MonadBaseControl b (LoggingT m) where
#if MIN_VERSION_monad_control(1,0,0)
     type StM (LoggingT m) a = StM m a
     liftBaseWith f = LoggingT $ \reader' ->
         liftBaseWith $ \runInBase ->
             f $ runInBase . (\(LoggingT r) -> r reader')
     restoreM = LoggingT . const . restoreM
#else
     newtype StM (LoggingT m) a = StMT (StM m a)
     liftBaseWith f = LoggingT $ \reader' ->
         liftBaseWith $ \runInBase ->
             f $ liftM StMT . runInBase . (\(LoggingT r) -> r reader')
     restoreM (StMT base) = LoggingT $ const $ restoreM base
#endif

instance MonadIO m => MonadLogger (LoggingT m) where
    monadLoggerLog a b c d = LoggingT $ \f -> liftIO $ f a b c (toLogStr d)
instance MonadIO m => MonadLoggerIO (LoggingT m) where
    askLoggerIO = LoggingT return

defaultOutput :: Handle
              -> Loc
              -> LogSource
              -> LogLevel
              -> LogStr
              -> IO ()
defaultOutput h loc src level msg =
    S8.hPutStr h ls
  where
    ls = defaultLogStrBS loc src level msg
defaultLogStrBS :: Loc
                -> LogSource
                -> LogLevel
                -> LogStr
                -> S8.ByteString
defaultLogStrBS a b c d =
    toBS $ defaultLogStr a b c d
  where
#if MIN_VERSION_fast_logger(2, 1, 0)
    toBS = fromLogStr
#elif MIN_VERSION_bytestring(0, 10, 2)
    toBS = L.toStrict . toLazyByteString . logStrBuilder
#else
    toBS = toByteString . logStrBuilder
#endif

defaultLogLevelStr :: LogLevel -> LogStr
defaultLogLevelStr level = case level of
    LevelOther t -> toLogStr t
    _            -> toLogStr $ S8.pack $ drop 5 $ show level

defaultLogStr :: Loc
              -> LogSource
              -> LogLevel
              -> LogStr
#if MIN_VERSION_fast_logger(0, 2, 0)
              -> LogStr
#else
              -> S8.ByteString
#endif
defaultLogStr loc src level msg =
#if MIN_VERSION_fast_logger(0, 2, 0)
    "[" `mappend` defaultLogLevelStr level `mappend`
    (if T.null src
        then mempty
        else "#" `mappend` toLogStr src) `mappend`
    "] " `mappend`
    msg `mappend`
    (if isDefaultLoc loc
        then "\n"
        else
            " @(" `mappend`
            toLogStr (S8.pack (fileLocStr loc)) `mappend`
            ")\n")
#else
    S8.concat
        [ S8.pack "["
        , case level of
            LevelOther t -> encodeUtf8 t
            _ -> encodeUtf8 $ pack $ drop 5 $ show level
        , if T.null src
            then S8.empty
            else encodeUtf8 $ '#' `T.cons` src
        , S8.pack "] "
        , case msg of
            LS s -> encodeUtf8 $ pack s
            LB b -> b
        , S8.pack " @("
        , encodeUtf8 $ pack (fileLocStr loc)
        , S8.pack ")\n"
        ]
#endif

-- taken from file-location package
-- turn the TH Loc loaction information into a human readable string
-- leaving out the loc_end parameter
fileLocStr :: Loc -> String
fileLocStr loc = (loc_package loc) ++ ':' : (loc_module loc) ++
  ' ' : (loc_filename loc) ++ ':' : (line loc) ++ ':' : (char loc)
  where
    line = show . fst . loc_start
    char = show . snd . loc_start

defaultTimedLogStr :: Loc
              -> LogSource
              -> LogLevel
              -> LogStr
              -> FormattedTime
#if MIN_VERSION_fast_logger(0, 2, 0)
              -> LogStr
#else
              -> S8.ByteString
#endif
defaultTimedLogStr loc src level msg time =
#if MIN_VERSION_fast_logger(0, 2, 0)
    "[" `mappend` defaultLogLevelStr level `mappend`
    (if T.null src
        then mempty
        else "#" `mappend` toLogStr src) `mappend`
    "] " `mappend` "[" `mappend` toLogStr time `mappend` "] " `mappend`
    msg `mappend`
    (if isDefaultLoc loc
        then "\n"
        else
            " @(" `mappend`
            toLogStr (S8.pack (fileLocStr loc)) `mappend`
            ")\n")
#else
    S8.concat
        [ S8.pack "["
        , case level of
            LevelOther t -> encodeUtf8 t
            _ -> encodeUtf8 $ pack $ drop 5 $ show level
        , if T.null src
            then S8.empty
            else encodeUtf8 $ '#' `T.cons` src
        , S8.pack "] "
        , S8.pack "["
        , time
        , S8.pack "] "
        , case msg of
            LS s -> encodeUtf8 $ pack s
            LB b -> b
        , S8.pack " @("
        , encodeUtf8 $ pack (fileLocStr loc)
        , S8.pack ")\n"
        ]
#endif

{-
defaultLogStrWithoutLoc ::
    LogSource -> LogLevel -> LogStr -> LogStr
defaultLogStrWithoutLoc loc src level msg =
    "[" `mappend` defaultLogLevelStr level `mappend`
    (if T.null src
        then mempty
        else "#" `mappend` toLogStr src) `mappend`
    "] " `mappend`
    msg `mappend` "\n"
-}


-- | Run a block using a @MonadLogger@ instance which prints to stderr.
--
-- Since 0.2.2
runStderrLoggingT :: MonadIO m => LoggingT m a -> m a
runStderrLoggingT = (`runLoggingT` defaultOutput stderr)

-- | Run a block using a @MonadLogger@ instance which prints to stdout.
--
-- Since 0.2.2
runStdoutLoggingT :: MonadIO m => LoggingT m a -> m a
runStdoutLoggingT = (`runLoggingT` defaultOutput stdout)

-- | Run a block using a @MonadLogger@ instance which writes tuples to an
--   unbounded channel.
--
--   The tuples can be extracted (ie. in another thread) with `unChanLoggingT`
--   or a custom extraction funtion, and written to a destination.
--
-- @since 0.3.17
runChanLoggingT :: MonadIO m => Chan (Loc, LogSource, LogLevel, LogStr) -> LoggingT m a -> m a
runChanLoggingT chan = (`runLoggingT` sink chan)
    where
        sink chan loc src lvl msg = writeChan chan (loc,src,lvl,msg)

-- | Read logging tuples from an unbounded channel and log them into a
--   `MonadLoggerIO` monad, forever.
--
--   For use in a dedicated thread with a channel fed by `runChanLoggingT`.
--
-- @since 0.3.17
unChanLoggingT :: (MonadLogger m, MonadIO m) => Chan (Loc, LogSource, LogLevel, LogStr) -> m ()
unChanLoggingT chan = forever $ do
    (loc,src,lvl,msg) <- liftIO $ readChan chan
    monadLoggerLog loc src lvl msg

-- | Within the 'LoggingT' monad, capture all log messages to a bounded
--   channel of the indicated size, and only actually log them if there is an
--   exception.
--
-- Since 0.3.2
withChannelLogger :: (MonadBaseControl IO m, MonadIO m)
                  => Int         -- ^ Number of messages to keep
                  -> LoggingT m a
                  -> LoggingT m a
withChannelLogger size action = LoggingT $ \logger -> do
    chan <- liftIO $ newTBChanIO size
    runLoggingT action (channelLogger chan logger) `onException` dumpLogs chan
  where
    channelLogger chan logger loc src lvl str = atomically $ do
        full <- isFullTBChan chan
        when full $ void $ readTBChan chan
        writeTBChan chan $ logger loc src lvl str

    dumpLogs chan = liftIO $
        sequence_ =<< atomically (untilM (readTBChan chan) (isEmptyTBChan chan))

-- | Run a block using a 'FastLogger'.
runFastLoggerLoggingT :: MonadIO m => FastLogger -> LoggingT m a -> m a
runFastLoggerLoggingT fl m = runLoggingT m $ \a b c d -> fl (defaultLogStr a b c d)

-- | Run a block using a 'TimedFastLogger'.
runTimedFastLoggerLoggingT :: MonadIO m => TimedFastLogger -> LoggingT m a -> m a
runTimedFastLoggerLoggingT tfl m = runLoggingT m $ \a b c d -> tfl (defaultTimedLogStr a b c d)

-- | Only log messages passing the given predicate function.
--
-- This can be a convenient way, for example, to ignore debug level messages.
--
-- Since 0.3.13
filterLogger :: (LogSource -> LogLevel -> Bool)
             -> LoggingT m a
             -> LoggingT m a
filterLogger p (LoggingT f) = LoggingT $ \logger ->
    f $ \loc src level msg ->
        when (p src level) $ logger loc src level msg

instance MonadCont m => MonadCont (LoggingT m) where
  callCC f = LoggingT $ \i -> callCC $ \c -> runLoggingT (f (LoggingT . const . c)) i

instance MonadError e m => MonadError e (LoggingT m) where
  throwError = Trans.lift . throwError
  catchError r h = LoggingT $ \i -> runLoggingT r i `catchError` \e -> runLoggingT (h e) i

instance MonadError e m => MonadError e (NoLoggingT m) where
  throwError = Trans.lift . throwError
  catchError r h = NoLoggingT $ runNoLoggingT r `catchError` \e -> runNoLoggingT (h e)

instance MonadRWS r w s m => MonadRWS r w s (LoggingT m)

instance MonadReader r m => MonadReader r (LoggingT m) where
  ask = Trans.lift ask
  local = mapLoggingT . local

mapLoggingT :: (m a -> n b) -> LoggingT m a -> LoggingT n b
mapLoggingT f = LoggingT . (f .) . runLoggingT

instance MonadState s m => MonadState s (LoggingT m) where
  get = Trans.lift get
  put = Trans.lift . put

instance MonadWriter w m => MonadWriter w (LoggingT m) where
  tell   = Trans.lift . tell
  listen = mapLoggingT listen
  pass   = mapLoggingT pass

mapNoLoggingT :: (m a -> n b) -> NoLoggingT m a -> NoLoggingT n b
mapNoLoggingT f = NoLoggingT . f . runNoLoggingT

instance MonadState s m => MonadState s (NoLoggingT m) where
    get = Trans.lift get
    put = Trans.lift . put

instance MonadWriter w m => MonadWriter w (NoLoggingT m) where
    tell   = Trans.lift . tell
    listen = mapNoLoggingT listen
    pass   = mapNoLoggingT pass

defaultLoc :: Loc
defaultLoc = Loc "<unknown>" "<unknown>" "<unknown>" (0,0) (0,0)

isDefaultLoc :: Loc -> Bool
isDefaultLoc (Loc "<unknown>" "<unknown>" "<unknown>" (0,0) (0,0)) = True
isDefaultLoc _ = False

logWithoutLoc :: (MonadLogger m, ToLogStr msg) => LogSource -> LogLevel -> msg -> m ()
logWithoutLoc = monadLoggerLog defaultLoc

logDebugN :: MonadLogger m => Text -> m ()
logDebugN = logWithoutLoc "" LevelDebug

logInfoN :: MonadLogger m => Text -> m ()
logInfoN = logWithoutLoc "" LevelInfo

logWarnN :: MonadLogger m => Text -> m ()
logWarnN = logWithoutLoc "" LevelWarn

logErrorN :: MonadLogger m => Text -> m ()
logErrorN = logWithoutLoc "" LevelError

logOtherN :: MonadLogger m => LogLevel -> Text -> m ()
logOtherN = logWithoutLoc ""

logDebugNS :: MonadLogger m => Text -> Text -> m ()
logDebugNS src = logWithoutLoc src LevelDebug

logInfoNS :: MonadLogger m => Text -> Text -> m ()
logInfoNS src = logWithoutLoc src LevelInfo

logWarnNS :: MonadLogger m => Text -> Text -> m ()
logWarnNS src = logWithoutLoc src LevelWarn

logErrorNS :: MonadLogger m => Text -> Text -> m ()
logErrorNS src = logWithoutLoc src LevelError

logOtherNS :: MonadLogger m => Text -> LogLevel -> Text -> m ()
logOtherNS = logWithoutLoc

