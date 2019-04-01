{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RankNTypes #-}

module AppState where

-- import GHC.Generics
import Control.Lens hiding ((.=))
import Data.Maybe
import Data.Aeson
import Data.Bifunctor (first)
import Data.List (find)
import Data.Semigroup (Max(..))
import qualified Data.List as L
import qualified Data.Text as T
import qualified Data.ByteString as BS
import Control.Monad.State.Strict
import qualified Data.Map.Strict as M

import Util (adjustMatching)
import qualified Game as G

type Id = Int
type UserId = T.Text

data User = User {
  _userId :: UserId,
  _username :: Maybe T.Text
} deriving (Eq, Show)

makeLenses ''User

initialUser userId = User { _userId = userId, _username = Nothing }

data Invite = Invite {
  _inviteId :: Maybe Id,
  _player1 :: UserId,
  _player2 :: UserId
} deriving (Eq, Show)

makeLenses ''Invite

instance FromJSON Invite where
  parseJSON = withObject "Invite" $ \v -> Invite
    <$> v .: "id"
    <*> v .: "player1"
    <*> v .: "player2"

instance ToJSON Invite where
  toJSON invite = object [
      "id" .= (invite^.inviteId),
      "player1" .= (invite^.player1),
      "player2" .= (invite^.player2)
    ]

inviteBelongsToUser :: UserId -> Invite -> Bool
inviteBelongsToUser userId invite = invite^.player1 == userId || invite^.player2 == userId

type GameId = Int

data GameAppState = GameAppState {
  _gameAppStateId :: GameId,
  _gameAppStatePlayers :: (UserId, UserId),
  _gameAppStateState :: G.State
} deriving (Eq, Show)

makeLenses ''GameAppState

instance ToJSON GameAppState where
  toJSON gameAppState = object [
      "id" .= (gameAppState^.gameAppStateId),
      "players" .= (gameAppState^.gameAppStatePlayers),
      "state" .= (gameAppState^.gameAppStateState)
    ]

data AppState = AppState {
  _users :: M.Map UserId User,
  _invites :: M.Map Id Invite,
  _gameAppStates :: M.Map GameId GameAppState
} deriving (Eq, Show)

makeLenses ''AppState

predicateToAtLike :: (a -> Bool) -> Lens' [a] (Maybe a)
predicateToAtLike pred = lens getter setter
  where
    getter = find pred
    setter val mNewVal = adjustMatching pred (const mNewVal) val

userById :: UserId -> Lens' AppState (Maybe User)
userById userId' = users . at userId'

(&&&) :: (a -> b) -> (a -> c) -> a -> (b, c)
(f1 &&& f2) v = (f1 v, f2 v)

userByUsername :: T.Text -> Lens' AppState (Maybe User)
userByUsername username' = users . mapListIso . predicateToAtLike pred
  where
    mapListIso :: Iso' (M.Map UserId User) [User]
    mapListIso = iso M.elems (M.fromList . map (_userId &&& id))

    pred :: User -> Bool
    pred u = u^.username == Just username'

initialAppState :: AppState
initialAppState = AppState { _users = M.empty, _invites = M.empty, _gameAppStates = M.empty }

getUserById :: UserId -> AppState -> Maybe User
getUserById userId' appState = find (\u -> u^.userId == userId') (appState^.users)

hasUser :: UserId -> AppState -> Bool
hasUser userId' appState = any (\u -> u^.userId == userId') (appState^.users)

addUser :: User -> AppState -> AppState
addUser user appState = appState & users %~ M.insert (user^.userId) user

updateUser :: User -> AppState -> AppState
updateUser user = (userById uid)._Just .~ user
  where uid = _userId user

ensureUser :: UserId -> AppState -> (User, AppState)
ensureUser userId = runState $ do
  hasUser' <- gets $ hasUser userId

  unless hasUser' $
    users %= M.insert userId (initialUser userId)

  gets $ fromJust . getUserById userId

setUserUsername :: UserId -> T.Text -> AppState -> (Maybe User, AppState)
setUserUsername userId newUsername = userById userId <%~ _Just . username ?~ newUsername

addInvite :: Invite -> AppState -> (Id, AppState)
addInvite inv appState = (invId, newAppState)
  where
    invId = nextId (appState^..invites.traverse.inviteId._Just)
    newInv = inv & inviteId .~ Just invId
    newAppState = appState & invites %~ M.insert invId newInv

deleteInvite :: Id -> AppState -> AppState
deleteInvite invId = invites . (at invId) .~ Nothing

findInvitesForUser :: UserId -> AppState -> [Invite]
findInvitesForUser userId = filter (inviteBelongsToUser userId) . M.elems . _invites

foldDefault :: Semigroup s => s -> [s] -> s
foldDefault = foldr (<>)

nextId = succ . getMax . foldDefault (Max 0) . map Max

nextGameId :: AppState -> GameId
nextGameId = nextId . M.keys . _gameAppStates

createGame :: UserId -> UserId -> AppState -> (GameAppState, AppState)
createGame uid otherUid appState =
  let
    newGameId = nextGameId appState
    gas = GameAppState newGameId (uid, otherUid) G.initialState
    newAppState = appState & gameAppStates %~ M.insert newGameId gas
  in
    (gas, newAppState)

acceptInvite :: Invite -> AppState -> (Maybe GameAppState, AppState)
acceptInvite inv = runState $ do
  let
    mInvId :: Maybe Id
    mInvId = inv^.inviteId

  case mInvId of
    Nothing -> return Nothing
    Just invId -> do
      hasInv <- uses invites (inv `elem`)

      if hasInv then do
        invites %= M.delete invId
        newGameId <- gets nextGameId

        let game = GameAppState newGameId (inv^.player1, inv^.player2) G.initialState

        gameAppStates %= M.insert newGameId game
        return $ Just game

      else return Nothing

-- Test stuff
testAppState :: AppState
testAppState = initialAppState { _users = M.fromList [("user1", testAuth "user1"), ("user2", testAuth "user2"), ("user3", testAuth "user3")] }

testAuth :: T.Text -> User
testAuth "user1" = User { _userId = "user1", _username = Nothing }
testAuth "user2" = User { _userId = "user2", _username = Just "testuser2" }
testAuth "user3" = User { _userId = "user3", _username = Just "anotheruser" }
