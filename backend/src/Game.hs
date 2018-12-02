{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE DeriveGeneric #-}

module Game where

import GHC.Generics
import Control.Lens

data State = State {
  _playerStates :: (PlayerState, PlayerState),
  _currentPlayer :: Player,
  _lastRoll :: Maybe Int
} deriving (Eq, Show, Generic)

data Player = Player1 | Player2 deriving (Eq, Show, Generic)

data PlayerState = PlayerState {
  _wonPieces :: Int,
  _outOfPlayPieces :: Int,
  _fieldedPieces :: [Piece]
} deriving (Eq, Show, Generic)

data Piece = Piece { _position :: Int } deriving (Eq, Show, Generic)

makeLenses ''State
makeLenses ''PlayerState
makeLenses ''Piece

currentPlayerState :: Lens' State PlayerState
currentPlayerState = lens getter setter
  where
    getter state = state ^. (subLens state)
    setter state newPlayerState = state & (subLens state) .~ newPlayerState

    subLens state = playerStateOf $ state^.currentPlayer

playerStateOf :: Player -> Lens' State PlayerState
playerStateOf Player1 = playerStates._1
playerStateOf Player2 = playerStates._2

opponentOf Player1 = Player2
opponentOf Player2 = Player1

next = opponentOf
