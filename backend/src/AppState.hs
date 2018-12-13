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

data User = User {
  _userId :: UserId,
  _userName :: Maybe T.Text
} deriving (Eq, Show)

data AppState = AppState {
  _users :: M.Map UserId User
} deriving (Eq, Show)

makeLenses ''User
makeLenses ''AppState

instance ToJSON User where
  toJSON user = object [ "id" .= (user^.userId), "name" .= (user^.userName) ]

userById userId = lens getter setter
  where
    setter appState newUser = appState & users %~ M.insert userId newUser
    getter appState = fromJust $ M.lookup userId (appState^.users)

initialUser :: UserId -> User
initialUser id = User { _userId = id, _userName = Nothing }

initialAppState :: AppState
initialAppState = AppState { _users = M.empty }

ensureUser :: UserId -> AppState -> (User, AppState)
ensureUser userId origAppState =
  case M.lookup userId (origAppState^.users) of
    Nothing ->
      let
        user = initialUser userId
        newAppState = origAppState & users %~ M.insert userId user
      in (user, newAppState)

    Just user -> (user, origAppState)
