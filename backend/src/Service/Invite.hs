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
  appState <- liftIO . readTVarIO $ appStateTVar

  let result = getInvitesLogic user appState

  case result of
    Left (status, msg) -> sendErrorAndFinish status msg
    Right invite -> S.json invite

getInvitesLogic :: User -> AppState -> Either (Status, T.Text) [Invite]
getInvitesLogic user appState =
  let
    userId' = user ^. AS.userId
  in
    AS.findInvitesForUser userId' appState <$ requireUsernameE user

createInvite :: ActionM User -> TVar AppState -> ActionM ()
createInvite auth appStateTVar = do
  user <- auth
  body <- S.jsonData

  let
    getAppState = readTVar appStateTVar
    modifyAppState = modifyTVar appStateTVar

  result <- liftIO . atomically $ createInviteLogic user body getAppState modifyAppState

  case result of
    Left (status, msg) -> sendErrorAndFinish status msg
    Right invite -> S.json invite

createInviteLogic :: Monad m =>
  User ->
  InviteBody ->
  m AppState ->
  ((AppState -> AppState) -> m a) ->
  m (Either (Status, T.Text) Invite)
createInviteLogic user body getAppState modifyAppState = E.runExceptT $ do
  E.liftEither $ requireUsernameE user

  let
    userId' = user ^. userId

    otherUserLens = case body of
      InviteBodyUserId uId -> userById uId
      InviteBodyUsername uName -> userByUsername uName

  appState <- E.lift $ getAppState

  case appState ^? otherUserLens . _Just . userId of
    Nothing -> E.throwError (badRequest400, ("No such user" :: T.Text))

    Just otherUserId -> do
      let invite = Invite { _player1 = userId', _player2 = otherUserId }

      E.lift $ modifyAppState $ addInvite invite

      return invite
