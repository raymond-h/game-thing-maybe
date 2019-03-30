{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Service.Invite where

import Control.Applicative
import Control.Concurrent.STM hiding (check)
import Control.Lens hiding ((.=))
import Control.Monad
import qualified Control.Monad.Except as E
import Control.Monad.Trans (liftIO)
import Data.Aeson
import Data.Foldable
import Data.Maybe
import Network.HTTP.Types
import Web.Scotty as S
import qualified Data.Text as T

import AppState as AS
import AuthUtil (requireUsernameE)
import Util (stateTVar, sendErrorAndFinish, guardError)

data InviteBody =
  InviteBodyUserId UserId |
  InviteBodyUsername T.Text
  deriving (Eq, Show)

instance FromJSON InviteBody where
  parseJSON = withObject "InviteBody with username or user ID" $ \v -> asum [
      InviteBodyUserId <$> v .: "userId",
      InviteBodyUsername <$> v .: "username"
    ]

getInvites :: ActionM User -> TVar AppState -> ActionM ()
getInvites auth appStateTVar = do
  user <- auth

  let lookupInvites uid = AS.findInvitesForUser uid <$> readTVarIO appStateTVar

  result <- liftIO $ getInvitesLogic lookupInvites user

  case result of
    Left (status, msg) -> sendErrorAndFinish status msg
    Right invite -> S.json invite

getInvitesLogic :: Monad m =>
  (UserId -> m [Invite]) ->
  User ->
  m (Either (Status, T.Text) [Invite])
getInvitesLogic lookupInvites user = E.runExceptT $ do
  E.liftEither $ requireUsernameE user

  E.lift $ lookupInvites (user^.userId)

createInvite :: ActionM User -> TVar AppState -> ActionM ()
createInvite auth appStateTVar = do
  user <- auth
  body <- S.jsonData

  let
    lookupUser criteria = do
      appState <- readTVar appStateTVar
      case criteria of
        ById userId' -> return $ appState ^. userById userId'
        ByUsername uname -> return $ appState ^. userByUsername uname

    addInvite inv = modifyTVar' appStateTVar $ AS.addInvite inv

  result <- liftIO . atomically $ createInviteLogic lookupUser addInvite user body

  case result of
    Left (status, msg) -> sendErrorAndFinish status msg
    Right invite -> S.json invite

data LookupCriteria = ById UserId | ByUsername T.Text

createInviteLogic :: Monad m =>
  (LookupCriteria -> m (Maybe User)) ->
  (Invite -> m ()) ->
  User ->
  InviteBody ->
  m (Either (Status, T.Text) Invite)
createInviteLogic lookupUser addInvite user body = E.runExceptT $ do
  E.liftEither $ requireUsernameE user

  let
    userId' = user ^. userId

    lookupCriteria = case body of
      InviteBodyUserId uId -> ById uId
      InviteBodyUsername uName -> ByUsername uName

  mOtherUser <- E.lift $ lookupUser lookupCriteria

  case mOtherUser of
    Nothing -> E.throwError (badRequest400, "No such user")

    Just otherUser -> do
      let
        otherUserId = otherUser ^. userId
        invite = Invite { _player1 = userId', _player2 = otherUserId }

      E.lift $ addInvite invite

      return invite
