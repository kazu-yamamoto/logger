{-# LANGUAGE OverloadedStrings #-}

-- | Logging system for WAI applications.
--
-- Sample code:
--
-- > {-# LANGUAGE OverloadedStrings #-}
-- > module Main where
-- >
-- > import Blaze.ByteString.Builder (fromByteString)
-- > import Control.Monad.IO.Class (liftIO)
-- > import Data.ByteString.Char8
-- > import Network.HTTP.Types (status200)
-- > import Network.Wai
-- > import Network.Wai.Handler.Warp
-- > import Network.Wai.Logger
-- > import System.IO
-- > import System.Log.FastLogger
-- >
-- > main :: IO ()
-- > main = do
-- >     dref <- dateInit
-- >     run 3000 $ logapp dref
-- >
-- > logapp :: DateRef -> Application
-- > logapp dref req = do
-- >     date <- liftIO $ getDate dref
-- >     let status = status200
-- >         len = 4
-- >     liftIO $ hPutLogStr stdout $ apacheFormat FromSocket date req status (Just len)
-- >     liftIO $ hFlush stdout
-- >     return $ ResponseBuilder status
-- >         [("Content-Type", "text/plain")
-- >         ,("Content-Length", pack (show len))]
-- >         $ fromByteString "PONG"

module Network.Wai.Logger (
    module Network.Wai.Logger.Format
  , module Network.Wai.Logger.Date
  , module Network.Wai.Logger.Utils
  ) where

import Network.Wai.Logger.Date
import Network.Wai.Logger.Format
import Network.Wai.Logger.Utils

