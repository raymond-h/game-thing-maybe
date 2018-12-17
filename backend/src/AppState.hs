{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module AppState where

import GHC.Generics
import Control.Lens hiding ((.=))
import Data.Maybe
import Data.Aeson
import qualified Data.Text as T
import qualified Data.ByteString as BS
import Control.Monad.State.Strict
import qualified Data.Map.Strict as M

type UserId = T.Text

data Invite = Invite {
  _player1 :: UserId,
  _player2 :: UserId
} deriving (Eq, Show)

makeLenses ''Invite

instance FromJSON Invite where
  parseJSON = withObject "Invite" $ \v -> Invite
    <$> v .: "player1"
    <*> v .: "player2"

instance ToJSON Invite where
  toJSON invite = object [
      "player1" .= (invite^.player1),
      "player2" .= (invite^.player2)
    ]

inviteBelongsToUser :: UserId -> Invite -> Bool
inviteBelongsToUser userId invite = invite^.player1 == userId || invite^.player2 == userId

data AppState = AppState {
  _invites :: [Invite]
} deriving (Eq, Show)

makeLenses ''AppState

initialAppState :: AppState
initialAppState = AppState { _invites = [] }

addInvite :: Invite -> AppState -> (Invite, AppState)
addInvite inv = runState $ do
  invites %= (++[inv])
  return inv

findInvitesForUser :: UserId -> AppState -> [Invite]
findInvitesForUser userId appState = filter (inviteBelongsToUser userId) (appState ^. invites)

