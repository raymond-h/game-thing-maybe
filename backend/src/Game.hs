{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Game where

import GHC.Generics
import Control.Lens
import Data.Aeson hiding ((.=))
import Data.Maybe (isNothing, isJust)
import Control.Monad (guard, unless, when)
import qualified Control.Monad.State.Strict as S

import Database.Persist
import Database.Persist.Sql

import Util (aesonLensBridgeOpts, guardM)

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

data Action = ActionSetDiceRolls Int | ActionAddPiece | ActionMovePiece Int | ActionPass deriving (Eq, Show, Generic)

data Move = MoveRollDice | MoveAddPiece | MoveMovePiece Int | MovePass deriving (Eq, Show, Generic)

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

instance ToJSON Move where toJSON = genericToJSON $ aesonLensBridgeOpts "Move"
instance FromJSON Move where parseJSON = genericParseJSON $ aesonLensBridgeOpts "Move"

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

hasPieceAt :: Int -> Player -> State -> Bool
hasPieceAt pos player = anyOf ((playerStateOf player).playerStateFieldedPieces.traverse.piecePosition) (==pos)

isPositionOccupied :: Int -> State -> Bool
isPositionOccupied pos state = hasPieceAt pos Player1 state || hasPieceAt pos Player2 state

isRerollSpot pos = pos `elem` [3, 7, 13]
isDangerZone pos = pos `elem` [4..11]
isSafeSpot pos = pos == 7

isValidRoll :: Int -> Bool
isValidRoll = (`elem` [0..4])

applyAction :: Action -> State -> Maybe State
applyAction (ActionSetDiceRolls roll) = S.execStateT $ do
  guard $ isValidRoll roll
  guardM $ uses stateLastRoll isNothing
  stateLastRoll ?= roll

applyAction ActionAddPiece = S.execStateT $ do
  lastRoll <- S.lift =<< use stateLastRoll
  guard $ lastRoll /= 0

  guardM $ uses (currentPlayerState.playerStateOutOfPlayPieces) (>0)

  currentPlayer <- use stateCurrentPlayer
  let newPos = lastRoll-1

  guardM $ S.gets (not . hasPieceAt newPos currentPlayer)

  currentPlayerState.playerStateOutOfPlayPieces -= 1
  currentPlayerState.playerStateFieldedPieces <>= [Piece newPos]

  stateLastRoll .= Nothing
  unless (isRerollSpot newPos) $ stateCurrentPlayer %= opponentOf

applyAction (ActionMovePiece n) = S.execStateT $ do
  lastRoll <- S.lift =<< use stateLastRoll
  guard $ lastRoll /= 0

  piece <- S.lift =<< preuse (currentPlayerState.playerStateFieldedPieces.(ix n))
  let newPos = piece^.piecePosition + lastRoll

  guard $ newPos <= 14

  currentPlayer <- use stateCurrentPlayer
  guardM $ S.gets (not . hasPieceAt newPos currentPlayer)

  if newPos == 14 then do
    currentPlayerState.playerStateWonPieces += 1
    currentPlayerState.playerStateFieldedPieces %= filter (/=piece)

  else do
    let opponent = opponentOf currentPlayer

    isNewPosOccupiedByOpponent <- S.gets $ hasPieceAt newPos opponent

    when (isDangerZone newPos && isNewPosOccupiedByOpponent) $ do
      guard $ not (isSafeSpot newPos)

      (playerStateOf opponent).playerStateFieldedPieces %= filter (\p -> p^.piecePosition /= newPos)
      (playerStateOf opponent).playerStateOutOfPlayPieces += 1

    currentPlayerState.playerStateFieldedPieces.(ix n).piecePosition .= newPos

  stateLastRoll .= Nothing
  unless (isRerollSpot newPos) $ stateCurrentPlayer %= opponentOf

applyAction ActionPass = S.execStateT $ do
  guardM $ uses stateLastRoll isJust

  stateLastRoll .= Nothing
  stateCurrentPlayer %= opponentOf

moveToAction :: Monad m => m Int -> Move -> m Action
moveToAction randomDice MoveRollDice = ActionSetDiceRolls <$> randomDice
moveToAction _ MoveAddPiece = return ActionAddPiece
moveToAction _ (MoveMovePiece n) = return (ActionMovePiece n)
moveToAction _ MovePass = return ActionPass

performMove :: Monad m => m Int -> Move -> State -> m (Maybe State)
performMove randomDice move state = applyAction <$> moveToAction randomDice move <*> pure state

instance PersistFieldSql State where
  sqlType _ = SqlString

instance PersistField State where
  toPersistValue = toPersistValueJSON
  fromPersistValue = fromPersistValueJSON
