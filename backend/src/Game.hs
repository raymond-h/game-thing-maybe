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

import Util (aesonLensBridgeOpts)

data State = State {
  _statePlayerStates :: (PlayerState, PlayerState),
  _stateCurrentPlayer :: Player,
  _stateLastRoll :: Maybe Int
} deriving (Eq, Show, Generic)

data Player = Player1 | Player2 deriving (Eq, Show, Generic)

data PlayerState = PlayerState {
  _playerStateWonPieces :: Int,
  _playerStateOutOfPlayPieces :: Int,
  _playerStateFieldedPieces :: [Piece]
} deriving (Eq, Show, Generic)

newtype Piece = Piece { _piecePosition :: Int } deriving (Eq, Show, Generic)

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

instance ToJSON State where toJSON = genericToJSON $ aesonLensBridgeOpts "State"
instance FromJSON State where parseJSON = genericParseJSON $ aesonLensBridgeOpts "State"

instance ToJSON Player where toJSON = genericToJSON $ aesonLensBridgeOpts "Player"
instance FromJSON Player where parseJSON = genericParseJSON $ aesonLensBridgeOpts "Player"

instance ToJSON PlayerState where toJSON = genericToJSON $ aesonLensBridgeOpts "PlayerState"
instance FromJSON PlayerState where parseJSON = genericParseJSON $ aesonLensBridgeOpts "PlayerState"

instance ToJSON Piece where toJSON = genericToJSON $ aesonLensBridgeOpts "Piece"
instance FromJSON Piece where parseJSON = genericParseJSON $ aesonLensBridgeOpts "Piece"

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
