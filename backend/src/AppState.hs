{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}

module AppState where

import GHC.Generics
import Control.Lens hiding ((.=))
import Data.Maybe
import Data.Aeson
import Data.Text as T
import Data.ByteString as BS
import qualified Data.Map.Strict as M

type UserId = T.Text

data AppState = AppState {} deriving (Eq, Show)

makeLenses ''AppState

initialAppState :: AppState
initialAppState = AppState {}

ensureUser :: UserId -> AppState -> (UserId, AppState)
ensureUser userId origAppState = (userId, origAppState)
