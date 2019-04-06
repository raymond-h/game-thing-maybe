{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Game where

import GHC.Generics
import Control.Lens hiding ((.=))
import Data.Aeson

import Database.Persist
import Database.Persist.Sql

data State = State {
  _statePlayerStates :: (PlayerState, PlayerState),
  _stateCurrentPlayer :: Player,
  _stateLastRoll :: Maybe Int
} deriving (Eq, Show)

data Player = Player1 | Player2 deriving (Eq, Show)

data PlayerState = PlayerState {
  _playerStateWonPieces :: Int,
  _playerStateOutOfPlayPieces :: Int,
  _playerStateFieldedPieces :: [Piece]
} deriving (Eq, Show)

newtype Piece = Piece { _piecePosition :: Int } deriving (Eq, Show)

makeLenses ''State
makeLenses ''PlayerState
makeLenses ''Piece

initialPlayerState = PlayerState {
  _playerStateWonPieces = 0,
  _playerStateOutOfPlayPieces = 7,
  _playerStateFieldedPieces = []
}

initialState = State {
  _statePlayerStates = (initialPlayerState, initialPlayerState),
  _stateCurrentPlayer = Player1,
  _stateLastRoll = Nothing
}

instance ToJSON State where
  toJSON state = object [
      "playerStates" .= (state ^. statePlayerStates),
      "currentPlayer" .= (state ^. stateCurrentPlayer),
      "lastRoll" .= (state ^. stateLastRoll)
    ]

instance FromJSON State where
  parseJSON = withObject "State" $ \v -> State
    <$> v .: "playerStates"
    <*> v .: "currentPlayer"
    <*> v .: "lastRoll"

instance ToJSON Player where
  toJSON Player1 = "player1"
  toJSON Player2 = "player2"

instance FromJSON Player where
  parseJSON = withText "Player" $ \v ->
    case v of
      "player1" -> return Player1
      "player2" -> return Player2

instance ToJSON PlayerState where
  toJSON playerState = object [
      "wonPieces" .= (playerState ^. playerStateWonPieces),
      "outOfPlayPieces" .= (playerState ^. playerStateOutOfPlayPieces),
      "fieldedPieces" .= (playerState ^. playerStateFieldedPieces)
    ]

instance FromJSON PlayerState where
  parseJSON = withObject "PlayerState" $ \v -> PlayerState
    <$> v .: "wonPieces"
    <*> v .: "outOfPlayPieces"
    <*> v .: "fieldedPieces"

instance ToJSON Piece where
  toJSON piece = object [ "position" .= (piece ^. piecePosition) ]

instance FromJSON Piece where
  parseJSON = withObject "Piece" $ \v -> Piece
    <$> v .: "position"

currentPlayerState :: Lens' State PlayerState
currentPlayerState = lens getter setter
  where
    getter state = state ^. subLens state
    setter state newPlayerState = state & subLens state .~ newPlayerState

    subLens state = playerStateOf $ state^.stateCurrentPlayer

playerStateOf :: Player -> Lens' State PlayerState
playerStateOf Player1 = statePlayerStates._1
playerStateOf Player2 = statePlayerStates._2

opponentOf Player1 = Player2
opponentOf Player2 = Player1

next = opponentOf

instance PersistFieldSql State where
  sqlType _ = SqlString

instance PersistField State where
  toPersistValue = toPersistValueJSON
  fromPersistValue = fromPersistValueJSON
