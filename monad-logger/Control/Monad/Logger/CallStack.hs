-- | Log functions using CallStack support in place of Template Haskell
--
-- @since 0.3.19
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
--
-- @since 0.3.19
logDebug :: (HasCallStack, Log.MonadLogger m) => Text -> m ()
logDebug = Log.logDebugCS callStack

-- | See 'logDebug'
--
-- @since 0.3.19
logInfo :: (HasCallStack, Log.MonadLogger m) => Text -> m ()
logInfo = Log.logInfoCS callStack

-- | See 'logDebug'
--
-- @since 0.3.19
logWarn :: (HasCallStack, Log.MonadLogger m) => Text -> m ()
logWarn = Log.logWarnCS callStack

-- | See 'logDebug'
--
-- @since 0.3.19
logError :: (HasCallStack, Log.MonadLogger m) => Text -> m ()
logError = Log.logErrorCS callStack

-- | See 'logDebug'
--
-- @since 0.3.19
logOther :: (HasCallStack, Log.MonadLogger m) => Log.LogLevel -> Text -> m ()
logOther = Log.logOtherCS callStack
