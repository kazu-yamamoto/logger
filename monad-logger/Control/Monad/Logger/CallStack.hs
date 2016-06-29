module Control.Monad.Logger.CallStack (
    module Log
  , logDebug
  , logInfo
  , logWarn
  , logError
  ) where

import           Control.Monad.Logger as Log hiding (logDebug, logError,
                                              logInfo, logOther, logWarn)
import           Data.Text            (Text)
import           GHC.Stack

-- | Logs a message with the location provided by
-- an implicit 'CallStack'.
logDebug :: (HasCallStack, Log.MonadLogger m) => Text -> m ()
logDebug = Log.logDebugCS callStack

-- | See 'logDebug'
logInfo :: (HasCallStack, Log.MonadLogger m) => Text -> m ()
logInfo = Log.logInfoCS callStack

-- | See 'logDebug'
logWarn :: (HasCallStack, Log.MonadLogger m) => Text -> m ()
logWarn = Log.logWarnCS callStack

-- | See 'logDebug'
logError :: (HasCallStack, Log.MonadLogger m) => Text -> m ()
logError = Log.logErrorCS callStack

-- | See 'logDebug'
logOther :: (HasCallStack, Log.MonadLogger m) => Log.LogLevel -> Text -> m ()
logOther = Log.logOtherCS callStack
