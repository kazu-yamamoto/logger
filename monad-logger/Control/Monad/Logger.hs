{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE CPP #-}
module Control.Monad.Logger
    ( -- * MonadLogger
      MonadLogger(..)
    , LogLevel(..)
    , LogSource
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
    ) where

import Language.Haskell.TH.Syntax (Lift (lift), Q, Exp, Loc (Loc), qLocation)
import System.Log.FastLogger (ToLogStr)

import Data.Monoid (Monoid)

import Data.Functor.Identity (Identity)
import Control.Monad.ST (ST)
import qualified Control.Monad.ST.Lazy as Lazy (ST)

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

import qualified Control.Monad.Trans.RWS.Strict    as Strict ( RWST   )
import qualified Control.Monad.Trans.State.Strict  as Strict ( StateT )
import qualified Control.Monad.Trans.Writer.Strict as Strict ( WriterT )

import Control.Monad.Trans.Class (MonadTrans)
import qualified Control.Monad.Trans.Class as Trans

import Data.Text (Text, pack, unpack)

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
    monadLoggerLog :: ToLogStr msg => Loc -> LogLevel -> msg -> m ()

    monadLoggerLogSource :: ToLogStr msg => Loc -> LogSource -> LogLevel -> msg -> m ()
    monadLoggerLogSource loc _ level msg = monadLoggerLog loc level msg

instance MonadLogger IO          where monadLoggerLog _ _ _ = return ()
instance MonadLogger Identity    where monadLoggerLog _ _ _ = return ()
instance MonadLogger (ST s)      where monadLoggerLog _ _ _ = return ()
instance MonadLogger (Lazy.ST s) where monadLoggerLog _ _ _ = return ()

#define DEF monadLoggerLog a b c = Trans.lift $ monadLoggerLog a b c; monadLoggerLogSource a b c d = Trans.lift $ monadLoggerLogSource a b c d
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
instance MonadLogger m => MonadLogger (Strict.StateT s m) where DEF
instance (MonadLogger m, Monoid w) => MonadLogger (Strict.WriterT w m) where DEF
instance (MonadLogger m, Monoid w) => MonadLogger (Strict.RWST r w s m) where DEF
#undef DEF

logTH :: LogLevel -> Q Exp
logTH level =
    [|monadLoggerLog $(qLocation >>= liftLoc) $(lift level) . (id :: Text -> Text)|]

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

liftLoc :: Loc -> Q Exp
liftLoc (Loc a b c d e) = [|Loc $(lift a) $(lift b) $(lift c) $(lift d) $(lift e)|]

-- | Generates a function that takes a 'LogSource' and 'Text' and logs a 'LevelDebug' message. Usage:
--
-- > $logDebug "SomeSource" "This is a debug log message"
logDebugS :: Q Exp
logDebugS = [|\a b -> monadLoggerLogSource $(qLocation >>= liftLoc) a LevelDebug (b :: Text)|]

-- | See 'logDebugS'
logInfoS :: Q Exp
logInfoS = [|\a b -> monadLoggerLogSource $(qLocation >>= liftLoc) a LevelInfo (b :: Text)|]
-- | See 'logDebugS'
logWarnS :: Q Exp
logWarnS = [|\a b -> monadLoggerLogSource $(qLocation >>= liftLoc) a LevelWarn (b :: Text)|]
-- | See 'logDebugS'
logErrorS :: Q Exp
logErrorS = [|\a b -> monadLoggerLogSource $(qLocation >>= liftLoc) a LevelError (b :: Text)|]

-- | Generates a function that takes a 'LogSource', a level name and a 'Text' and logs a 'LevelOther' message. Usage:
--
-- > $logOther "SomeSource" "My new level" "This is a log message"
logOtherS :: Q Exp
logOtherS = [|\src level msg -> monadLoggerLogSource $(qLocation >>= liftLoc) src (LevelOther level) (msg :: Text)|]
