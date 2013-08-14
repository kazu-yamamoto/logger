{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UndecidableInstances #-}
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
    , LogLevel(..)
    , LogSource
    -- * Helper transformer
    , LoggingT
    , RLoggingT (..)
    , runStderrLoggingT
    , runStdoutLoggingT
    , NoLoggingT (..)
    -- * TH logging
    , logDebug
    , logInfo
    , logWarn
    , logError
    , logOther
    -- * TH logging with source
    , logDebugS
    , logInfoS
    , logWarnS
    , logErrorS
    , logOtherS
    -- * TH util
    , liftLoc
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
    ) where

import Language.Haskell.TH.Syntax (Lift (lift), Q, Exp, Loc (..), qLocation)
import System.Log.FastLogger (ToLogStr (toLogStr), LogStr (..))

import Data.Monoid (Monoid)

import Control.Applicative (Applicative (..))
import Control.Monad (liftM, ap)
import Control.Monad.Base (MonadBase (liftBase))
import Control.Monad.Trans.Control (MonadBaseControl (..), MonadTransControl (..))
import qualified Control.Monad.Trans.Class as Trans

import System.IO (stdout, stderr, Handle)

import Control.Monad.IO.Class (MonadIO (liftIO))
import Control.Monad.Trans.Resource (MonadResource (liftResourceT), MonadThrow (monadThrow))

import Control.Monad.Trans.Identity ( IdentityT)
import Control.Monad.Trans.List     ( ListT    )
import Control.Monad.Trans.Maybe    ( MaybeT   )
import Control.Monad.Trans.Error    ( ErrorT, Error)
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
import Data.Reflection

import Data.Text (Text, pack, unpack)
import qualified Data.Text as T
import qualified Data.ByteString.Char8 as S8
import Data.Text.Encoding (encodeUtf8)

import Control.Monad.Cont.Class   ( MonadCont (..) )
import Control.Monad.Error.Class  ( MonadError (..) )
import Control.Monad.RWS.Class    ( MonadRWS )
import Control.Monad.Reader.Class ( MonadReader (..) )
import Control.Monad.State.Class  ( MonadState (..) )
import Control.Monad.Writer.Class ( MonadWriter (..) )

data LogLevel = LevelDebug | LevelInfo | LevelWarn | LevelError | LevelOther Text
    deriving (Eq, Prelude.Show, Prelude.Read, Ord)

instance Lift LogLevel where
    lift LevelDebug = [|LevelDebug|]
    lift LevelInfo = [|LevelInfo|]
    lift LevelWarn = [|LevelWarn|]
    lift LevelError = [|LevelError|]
    lift (LevelOther x) = [|LevelOther $ pack $(lift $ unpack x)|]

type LogSource = Text

class Monad m => MonadLogger m where
    monadLoggerLog :: ToLogStr msg => Loc -> LogSource -> LogLevel -> msg -> m ()

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

logTH :: LogLevel -> Q Exp
logTH level =
    [|monadLoggerLog $(qLocation >>= liftLoc) (pack "") $(lift level) . (id :: Text -> Text)|]

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
-- > $logDebug "SomeSource" "This is a debug log message"
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
-- > $logOther "SomeSource" "My new level" "This is a log message"
logOtherS :: Q Exp
logOtherS = [|\src level msg -> monadLoggerLog $(qLocation >>= liftLoc) src (LevelOther level) (msg :: Text)|]

-- | Monad transformer that disables logging.
--
-- Since 0.3.2
type Logger m = Loc -> LogSource -> LogLevel -> LogStr -> m ()

newtype RLoggingT s m a = RLoggingT { runRLoggingT :: m a }

instance Monad m => Functor (RLoggingT s m) where
    fmap = liftM

instance Monad m => Applicative (RLoggingT s m) where
    pure = return
    (<*>) = ap

instance Monad m => Monad (RLoggingT s m) where
    return = RLoggingT . return
    RLoggingT ma >>= f = RLoggingT $ ma >>= runRLoggingT . f

instance MonadIO m => MonadIO (RLoggingT s m) where
    liftIO = Trans.lift . liftIO

instance MonadThrow m => MonadThrow (RLoggingT s m) where
    monadThrow = Trans.lift . monadThrow

instance MonadResource m => MonadResource (RLoggingT s m) where
    liftResourceT = Trans.lift . liftResourceT

instance MonadBase b m => MonadBase b (RLoggingT s m) where
    liftBase = Trans.lift . liftBase

instance Trans.MonadTrans (RLoggingT s) where
    lift = RLoggingT

instance MonadTransControl (RLoggingT s) where
    newtype StT (RLoggingT s) a = StIdent {unStIdent :: a}
    liftWith f = RLoggingT $ f $ \(RLoggingT t) -> liftM StIdent t
    restoreT = RLoggingT . liftM unStIdent
    {-# INLINE liftWith #-}
    {-# INLINE restoreT #-}

instance MonadBaseControl b m => MonadBaseControl b (RLoggingT s m) where
     newtype StM (RLoggingT s m) a = StMT' (StM m a)
     liftBaseWith f = RLoggingT $
         liftBaseWith $ \runInBase ->
             f $ liftM StMT' . runInBase . (\(RLoggingT r) -> r)
     restoreM (StMT' base) = RLoggingT $ restoreM base

instance (Monad m, Reifies s (Logger m)) => MonadLogger (RLoggingT s m) where
    monadLoggerLog a b c d = RLoggingT $ reflect (undefined :: proxy s) a b c (toLogStr d)

instance MonadCont m => MonadCont (RLoggingT s m) where
  callCC f = RLoggingT $ callCC $ \c -> runRLoggingT (f (RLoggingT . c))

instance MonadError e m => MonadError e (RLoggingT s m) where
  throwError = Trans.lift . throwError
  catchError r h = RLoggingT $ runRLoggingT r `catchError` \e -> runRLoggingT (h e)

instance MonadRWS r w s m => MonadRWS r w s (RLoggingT s m)

instance MonadReader r m => MonadReader r (RLoggingT s m) where
  ask = Trans.lift ask
  local = mapRLoggingT . local

mapRLoggingT :: (m a -> n b) -> RLoggingT s m a -> RLoggingT s n b
mapRLoggingT f = RLoggingT . f . runRLoggingT

instance MonadState s m => MonadState s (RLoggingT s m) where
  get = Trans.lift get
  put = Trans.lift . put

instance MonadWriter w m => MonadWriter w (RLoggingT s m) where
  tell   = Trans.lift . tell
  listen = mapRLoggingT listen
  pass   = mapRLoggingT pass

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

instance MonadThrow m => MonadThrow (NoLoggingT m) where
    monadThrow = Trans.lift . monadThrow

instance MonadResource m => MonadResource (NoLoggingT m) where
    liftResourceT = Trans.lift . liftResourceT

instance MonadBase b m => MonadBase b (NoLoggingT m) where
    liftBase = Trans.lift . liftBase

instance Trans.MonadTrans NoLoggingT where
    lift = NoLoggingT

instance MonadTransControl NoLoggingT where
    newtype StT NoLoggingT a = StRIdent {unStRIdent :: a}
    liftWith f = NoLoggingT $ f $ \(NoLoggingT t) -> liftM StRIdent t
    restoreT = NoLoggingT . liftM unStRIdent
    {-# INLINE liftWith #-}
    {-# INLINE restoreT #-}

instance MonadBaseControl b m => MonadBaseControl b (NoLoggingT m) where
     newtype StM (NoLoggingT m) a = StRMT' (StM m a)
     liftBaseWith f = NoLoggingT $
         liftBaseWith $ \runInBase ->
             f $ liftM StRMT' . runInBase . (\(NoLoggingT r) -> r)
     restoreM (StRMT' base) = NoLoggingT $ restoreM base

instance MonadIO m => MonadLogger (NoLoggingT m) where
    monadLoggerLog _ _ _ _ = return ()

-- | Monad transformer that adds a new logging function.
--
-- Since 0.2.2
type LoggingT m a = forall s. RLoggingT s m a

defaultOutput :: MonadIO m
              => Handle
              -> Loc
              -> LogSource
              -> LogLevel
              -> LogStr
              -> m ()
defaultOutput h loc src level msg =
    liftIO $ S8.hPutStrLn h $ S8.concat bs
  where
    bs =
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
        , encodeUtf8 $ pack fileLocStr
        , S8.pack ")\n"
        ]

    -- taken from file-location package
    -- turn the TH Loc loaction information into a human readable string
    -- leaving out the loc_end parameter
    fileLocStr = (loc_package loc) ++ ':' : (loc_module loc) ++
      ' ' : (loc_filename loc) ++ ':' : (line loc) ++ ':' : (char loc)
      where
        line = show . fst . loc_start
        char = show . snd . loc_start

-- | Run a block using a @MonadLogger@ instance which prints to stderr.
--
-- Since 0.3.2
runLoggingT :: MonadIO m => Logger m -> LoggingT m a -> m a
runLoggingT logger act = runRLoggingT $ reify logger (const act)

-- Since 0.2.2
runStderrLoggingT :: MonadIO m => LoggingT m a -> m a
runStderrLoggingT = runLoggingT (defaultOutput stderr)

-- | Run a block using a @MonadLogger@ instance which prints to stdout.
--
-- Since 0.2.2
runStdoutLoggingT :: MonadIO m => LoggingT m a -> m a
runStdoutLoggingT = runLoggingT (defaultOutput stdout)

defaultLoc :: Loc
defaultLoc = Loc "<unknown>" "<unknown>" "<unknown>" (0,0) (0,0)

logDebugN :: MonadLogger m => Text -> m ()
logDebugN msg =
    monadLoggerLog defaultLoc "" LevelDebug msg

logInfoN :: MonadLogger m => Text -> m ()
logInfoN msg =
    monadLoggerLog defaultLoc "" LevelInfo msg

logWarnN :: MonadLogger m => Text -> m ()
logWarnN msg =
    monadLoggerLog defaultLoc "" LevelWarn msg

logErrorN :: MonadLogger m => Text -> m ()
logErrorN msg =
    monadLoggerLog defaultLoc "" LevelError msg

logOtherN :: MonadLogger m => LogLevel -> Text -> m ()
logOtherN level msg =
    monadLoggerLog defaultLoc "" level msg

logDebugNS :: MonadLogger m => Text -> Text -> m ()
logDebugNS src msg =
    monadLoggerLog defaultLoc src LevelDebug msg

logInfoNS :: MonadLogger m => Text -> Text -> m ()
logInfoNS src msg =
    monadLoggerLog defaultLoc src LevelInfo msg

logWarnNS :: MonadLogger m => Text -> Text -> m ()
logWarnNS src msg =
    monadLoggerLog defaultLoc src LevelWarn msg

logErrorNS :: MonadLogger m => Text -> Text -> m ()
logErrorNS src msg =
    monadLoggerLog defaultLoc src LevelError msg

logOtherNS :: MonadLogger m => Text -> LogLevel -> Text -> m ()
logOtherNS src level msg =
    monadLoggerLog defaultLoc src level msg
