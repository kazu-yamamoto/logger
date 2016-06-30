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
import qualified Data.Text            as Text
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

-- | Logs a showable value with the location provided by
-- an implicit 'CallStack'.
--
-- @since 0.3.19
logDebugSH :: (HasCallStack, Log.MonadLogger m, Show a) => a -> m ()
logDebugSH = Log.logDebugCS callStack . Text.pack . show

-- | See 'logDebugSH'
--
-- @since 0.3.19
logInfoSH :: (HasCallStack, Log.MonadLogger m, Show a) => a -> m ()
logInfoSH = Log.logInfoCS callStack . Text.pack . show

-- | See 'logDebugSH'
--
-- @since 0.3.19
logWarnSH :: (HasCallStack, Log.MonadLogger m, Show a) => a -> m ()
logWarnSH = Log.logWarnCS callStack . Text.pack . show

-- | See 'logDebugSH'
--
-- @since 0.3.19
logErrorSH :: (HasCallStack, Log.MonadLogger m, Show a) => a -> m ()
logErrorSH = Log.logErrorCS callStack . Text.pack . show

-- | See 'logDebugSH'
--
-- @since 0.3.19
logOtherSH :: (HasCallStack, Log.MonadLogger m, Show a) => Log.LogLevel -> a -> m ()
logOtherSH lvl = Log.logOtherCS callStack lvl . Text.pack . show
