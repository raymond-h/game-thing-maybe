{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RankNTypes #-}

module AppState where

import GHC.Generics
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

import qualified Network.Pusher as P
-- import qualified PusherCommon as PC

import Util (adjustMatching, aesonLensBridgeOpts)
import qualified Game as G

type Id = Int
type UserId = T.Text

data User = User {
  _userId :: UserId,
  _userUsername :: Maybe T.Text
} deriving (Eq, Show, Generic)

makeLenses ''User

instance ToJSON User where toJSON = genericToJSON $ aesonLensBridgeOpts "User"
instance FromJSON User where parseJSON = genericParseJSON $ aesonLensBridgeOpts "User"

initialUser userId = User { _userId = userId, _userUsername = Nothing }

data Invite = Invite {
  _inviteId :: Maybe Id,
  _invitePlayer1 :: UserId,
  _invitePlayer2 :: UserId
} deriving (Eq, Show, Generic)

makeLenses ''Invite

instance ToJSON Invite where toJSON = genericToJSON $ aesonLensBridgeOpts "Invite"
instance FromJSON Invite where parseJSON = genericParseJSON $ aesonLensBridgeOpts "Invite"

inviteBelongsToUser :: UserId -> Invite -> Bool
inviteBelongsToUser userId invite = invite^.invitePlayer1 == userId || invite^.invitePlayer2 == userId

type GameId = Int

data GameAppState = GameAppState {
  _gameAppStateId :: GameId,
  _gameAppStatePlayers :: (UserId, UserId),
  _gameAppStateState :: G.State
} deriving (Eq, Show, Generic)

makeLenses ''GameAppState

instance ToJSON GameAppState where toJSON = genericToJSON $ aesonLensBridgeOpts "GameAppState"
instance FromJSON GameAppState where parseJSON = genericParseJSON $ aesonLensBridgeOpts "GameAppState"

data AppState = AppState {
  _users :: M.Map UserId User,
  _invites :: M.Map Id Invite,
  _gameAppStates :: M.Map GameId GameAppState,
  _pusherEventsSent :: [([P.Channel], P.Event, P.EventData)]
} deriving (Eq, Show, Generic)

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
    pred u = u^.userUsername == Just username'

initialAppState :: AppState
initialAppState = AppState { _users = M.empty, _invites = M.empty, _gameAppStates = M.empty, _pusherEventsSent = [] }

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
setUserUsername userId newUsername = userById userId <%~ _Just . userUsername ?~ newUsername

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
