{-# LANGUAGE TemplateHaskell #-}
module Control.Monad.Logger
    ( -- * MonadLogger
      MonadLogger(..)
    , LogLevel(..)
    -- * TH logging
    , logDebug
    , logInfo
    , logWarn
    , logError
    , logOther
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

class Monad m => MonadLogger m where
    monadLoggerLog :: ToLogStr msg => Loc -> LogLevel -> msg -> m ()

instance MonadLogger IO          where monadLoggerLog _ _ _ = return ()
instance MonadLogger Identity    where monadLoggerLog _ _ _ = return ()
instance MonadLogger (ST s)      where monadLoggerLog _ _ _ = return ()
instance MonadLogger (Lazy.ST s) where monadLoggerLog _ _ _ = return ()

liftLog :: (MonadTrans t, MonadLogger m, ToLogStr msg) => Loc -> LogLevel -> msg -> t m ()
liftLog a b c = Trans.lift $ monadLoggerLog a b c

instance MonadLogger m => MonadLogger (IdentityT m) where monadLoggerLog = liftLog
instance MonadLogger m => MonadLogger (ListT m) where monadLoggerLog = liftLog
instance MonadLogger m => MonadLogger (MaybeT m) where monadLoggerLog = liftLog
instance (MonadLogger m, Error e) => MonadLogger (ErrorT e m) where monadLoggerLog = liftLog
instance MonadLogger m => MonadLogger (ReaderT r m) where monadLoggerLog = liftLog
instance MonadLogger m => MonadLogger (ContT r m) where monadLoggerLog = liftLog
instance MonadLogger m => MonadLogger (StateT s m) where monadLoggerLog = liftLog
instance (MonadLogger m, Monoid w) => MonadLogger (WriterT w m) where monadLoggerLog = liftLog
instance (MonadLogger m, Monoid w) => MonadLogger (RWST r w s m) where monadLoggerLog = liftLog
instance MonadLogger m => MonadLogger (ResourceT m) where monadLoggerLog = liftLog
instance MonadLogger m => MonadLogger (Strict.StateT s m) where monadLoggerLog = liftLog
instance (MonadLogger m, Monoid w) => MonadLogger (Strict.WriterT w m) where monadLoggerLog = liftLog
instance (MonadLogger m, Monoid w) => MonadLogger (Strict.RWST r w s m) where monadLoggerLog = liftLog

logTH :: LogLevel -> Q Exp
logTH level =
    [|monadLoggerLog $(qLocation >>= liftLoc) $(lift level) . (id :: Text -> Text)|]
  where
    liftLoc :: Loc -> Q Exp
    liftLoc (Loc a b c d e) = [|Loc $(lift a) $(lift b) $(lift c) $(lift d) $(lift e)|]

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
