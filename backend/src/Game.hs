{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Game where

import GHC.Generics
import Control.Lens hiding ((.=))
import Data.Aeson

data State = State {
  _playerStates :: (PlayerState, PlayerState),
  _currentPlayer :: Player,
  _lastRoll :: Maybe Int
} deriving (Eq, Show)

data Player = Player1 | Player2 deriving (Eq, Show)

data PlayerState = PlayerState {
  _wonPieces :: Int,
  _outOfPlayPieces :: Int,
  _fieldedPieces :: [Piece]
} deriving (Eq, Show)

newtype Piece = Piece { _position :: Int } deriving (Eq, Show)

makeLenses ''State
makeLenses ''PlayerState
makeLenses ''Piece

initialPlayerState = PlayerState {
  _wonPieces = 0, _outOfPlayPieces = 7, _fieldedPieces = []
}

initialState = State {
  _playerStates = (initialPlayerState, initialPlayerState),
  _currentPlayer = Player1,
  _lastRoll = Nothing
}

instance ToJSON State where
  toJSON state = object [
      "playerStates" .= (state ^. playerStates),
      "currentPlayer" .= (state ^. currentPlayer),
      "lastRoll" .= (state ^. lastRoll)
    ]

instance ToJSON Player where
  toJSON Player1 = "player1"
  toJSON Player2 = "player2"

instance ToJSON PlayerState where
  toJSON playerState = object [
      "wonPieces" .= (playerState ^. wonPieces),
      "outOfPlayPieces" .= (playerState ^. outOfPlayPieces),
      "fieldedPieces" .= (playerState ^. fieldedPieces)
    ]

instance ToJSON Piece where
  toJSON piece = object [ "position" .= (piece ^. position) ]

currentPlayerState :: Lens' State PlayerState
currentPlayerState = lens getter setter
  where
    getter state = state ^. subLens state
    setter state newPlayerState = state & subLens state .~ newPlayerState

    subLens state = playerStateOf $ state^.currentPlayer

playerStateOf :: Player -> Lens' State PlayerState
playerStateOf Player1 = playerStates._1
playerStateOf Player2 = playerStates._2

opponentOf Player1 = Player2
opponentOf Player2 = Player1

next = opponentOf
