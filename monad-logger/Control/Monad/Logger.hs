{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
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
    , LoggingT (..)
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
    ) where

import Language.Haskell.TH.Syntax (Lift (lift), Q, Exp, Loc (..), qLocation)
import System.Log.FastLogger (ToLogStr (toLogStr), LogStr (..))

import Data.Monoid (Monoid)

import Control.Applicative (Applicative (..))
import Control.Monad (liftM, ap)
import Control.Monad.Base (MonadBase (liftBase))
import Control.Monad.Trans.Control (MonadBaseControl (..), MonadTransControl (..))
import Data.Functor.Identity (Identity)
import Control.Monad.ST (ST)
import qualified Control.Monad.ST.Lazy as Lazy (ST)
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

import Data.Text (Text, pack, unpack, empty)
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
-- Since 0.3.0
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
    newtype StT NoLoggingT a = StIdent {unStIdent :: a}
    liftWith f = NoLoggingT $ f $ \(NoLoggingT t) -> liftM StIdent t
    restoreT = NoLoggingT . liftM unStIdent
    {-# INLINE liftWith #-}
    {-# INLINE restoreT #-}

instance MonadBaseControl b m => MonadBaseControl b (NoLoggingT m) where
     newtype StM (NoLoggingT m) a = StMT' (StM m a)
     liftBaseWith f = NoLoggingT $
         liftBaseWith $ \runInBase ->
             f $ liftM StMT' . runInBase . (\(NoLoggingT r) -> r)
     restoreM (StMT' base) = NoLoggingT $ restoreM base

instance MonadIO m => MonadLogger (NoLoggingT m) where
    monadLoggerLog _ _ _ _ = return ()

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

instance MonadThrow m => MonadThrow (LoggingT m) where
    monadThrow = Trans.lift . monadThrow

instance MonadResource m => MonadResource (LoggingT m) where
    liftResourceT = Trans.lift . liftResourceT

instance MonadBase b m => MonadBase b (LoggingT m) where
    liftBase = Trans.lift . liftBase

instance Trans.MonadTrans LoggingT where
    lift = LoggingT . const

instance MonadTransControl LoggingT where
    newtype StT LoggingT a = StReader {unStReader :: a}
    liftWith f = LoggingT $ \r -> f $ \(LoggingT t) -> liftM StReader $ t r
    restoreT = LoggingT . const . liftM unStReader
    {-# INLINE liftWith #-}
    {-# INLINE restoreT #-}

instance MonadBaseControl b m => MonadBaseControl b (LoggingT m) where
     newtype StM (LoggingT m) a = StMT (StM m a)
     liftBaseWith f = LoggingT $ \reader' ->
         liftBaseWith $ \runInBase ->
             f $ liftM StMT . runInBase . (\(LoggingT r) -> r reader')
     restoreM (StMT base) = LoggingT $ const $ restoreM base

instance MonadIO m => MonadLogger (LoggingT m) where
    monadLoggerLog a b c d = LoggingT $ \f -> liftIO $ f a b c (toLogStr d)

defaultOutput :: Handle
              -> Loc
              -> LogSource
              -> LogLevel
              -> LogStr
              -> IO ()
defaultOutput h loc src level msg =
    S8.hPutStrLn h $ S8.concat bs
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
-- Since 0.2.2
runStderrLoggingT :: MonadIO m => LoggingT m a -> m a
runStderrLoggingT = (`runLoggingT` defaultOutput stderr)

-- | Run a block using a @MonadLogger@ instance which prints to stdout.
--
-- Since 0.2.2
runStdoutLoggingT :: MonadIO m => LoggingT m a -> m a
runStdoutLoggingT = (`runLoggingT` defaultOutput stdout)

instance MonadCont m => MonadCont (LoggingT m) where
  callCC f = LoggingT $ \i -> callCC $ \c -> runLoggingT (f (LoggingT . const . c)) i

instance MonadError e m => MonadError e (LoggingT m) where
  throwError = Trans.lift . throwError
  catchError r h = LoggingT $ \i -> runLoggingT r i `catchError` \e -> runLoggingT (h e) i

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
