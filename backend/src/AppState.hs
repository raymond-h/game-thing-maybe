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
import qualified Data.Text as T
import qualified Data.ByteString as BS
import Control.Monad.State.Strict
import qualified Data.Map.Strict as M

import Util (adjustMatching)

type UserId = T.Text

data User = User {
  _userId :: UserId,
  _username :: Maybe T.Text
} deriving (Eq, Show)

makeLenses ''User

initialUser userId = User { _userId = userId, _username = Nothing }

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
  _users :: [User],
  _invites :: [Invite]
} deriving (Eq, Show)

makeLenses ''AppState

predicateToAtLike :: (a -> Bool) -> Lens' [a] (Maybe a)
predicateToAtLike pred = lens getter setter
  where
    getter = find pred
    setter val mNewVal = adjustMatching pred (const mNewVal) val

userById :: UserId -> Lens' AppState (Maybe User)
userById userId' = users . predicateToAtLike isUser
  where
    isUser u = u^.userId == userId'

userByUsername :: UserId -> Lens' AppState (Maybe User)
userByUsername username' = users . predicateToAtLike isUser
  where
    isUser u = u^.username == Just username'

initialAppState :: AppState
initialAppState = AppState { _users = [], _invites = [] }

getUserById :: UserId -> AppState -> Maybe User
getUserById userId' appState = find (\u -> u^.userId == userId') (appState^.users)

hasUser :: UserId -> AppState -> Bool
hasUser userId' appState = any (\u -> u^.userId == userId') (appState^.users)

ensureUser :: UserId -> AppState -> (User, AppState)
ensureUser userId = runState $ do
  hasUser' <- gets $ hasUser userId

  when (not $ hasUser') $
    users %= (++[initialUser userId])

  gets $ fromJust . getUserById userId

modifyUser :: UserId -> (User -> User) -> AppState -> (User, AppState)
modifyUser userId userFn = runState $ do
  userById userId . traverse %= userFn

  fromJust <$> (preuse $ userById userId . traverse)

setUserUsername :: UserId -> T.Text -> AppState -> (Maybe User, AppState)
setUserUsername userId newUsername = userById userId <%~ _Just . username ?~ newUsername

addInvite :: Invite -> AppState -> AppState
addInvite inv = invites %~ (++[inv])

findInvitesForUser :: UserId -> AppState -> [Invite]
findInvitesForUser userId appState = filter (inviteBelongsToUser userId) (appState ^. invites)

-- Test stuff
testAppState :: AppState
testAppState = initialAppState { _users = [testAuth "user1", testAuth "user2", testAuth "user3"] }

testAuth :: T.Text -> User
testAuth "user1" = User { _userId = "user1", _username = Nothing }
testAuth "user2" = User { _userId = "user2", _username = Just "testuser2" }
testAuth "user3" = User { _userId = "user3", _username = Just "anotheruser" }
