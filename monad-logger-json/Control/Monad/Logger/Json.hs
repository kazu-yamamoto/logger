{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
module Control.Monad.Logger.Json where

-- logging imports
import Control.Monad.Logger
import System.Log.FastLogger

-- system imports
import System.IO (IO, Handle)
import qualified Data.ByteString.Char8 as S8
import qualified Data.ByteString.Lazy as BSL
import Data.Function ((&), ($), (.))
import Data.Monoid (mappend, (<>))
import Prelude
import GHC.Generics (Generic)

-- json imports
import Data.Aeson

jsonOutput :: Handle
              -> Loc
              -> LogSource
              -> LogLevel
              -> LogStr
              -> IO ()
jsonOutput h loc src level msg =
    BSL.hPutStr h ls
  where
    ls = jsonLogStrBS loc src level msg

data JsonLog = JsonLog Loc LogSource LogLevel LogStr
  deriving Generic

instance ToJSON JsonLog where
  toJSON (JsonLog loc src level msg) = object
    [ "loc" .= locJson loc
    , "source" .= src
    , "level" .= defaultLogLevelValue level
    , "msg" .= S8.unpack (fromLogStr msg)
    ]
  toEncoding (JsonLog loc src level msg) = pairs
    (  "loc" .= locJson loc
    <> "source" .= src
    <> "level" .= defaultLogLevelValue level
    <> "msg" .= S8.unpack (fromLogStr msg)
    )

locJson :: Loc -> String
locJson loc =
    if isDefaultLoc loc
        then ""
        else fileLocStr
  where
    -- omitting package on purpose
    fileLocStr = (loc_filename loc) <> (':' : (line loc)) <> (':' : (char loc))
      where
        line = show . fst . loc_start
        char = show . snd . loc_start

jsonLogStrBS :: Loc
              -> LogSource
              -> LogLevel
              -> LogStr
              -> BSL.ByteString
jsonLogStrBS loc src level msg =
    JsonLog loc src level msg
  & encode
  & (`mappend` "\n")


defaultLogLevelValue :: LogLevel -> Value
defaultLogLevelValue level = case level of
    LevelOther t -> toJSON t
    _            -> toJSON $ drop 5 $ show level
