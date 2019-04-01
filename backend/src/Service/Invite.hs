{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DeriveGeneric #-}

module Service.Invite where

import GHC.Generics
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
import qualified Validation as V
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

    addInvite inv = stateTVar appStateTVar $ AS.addInvite inv

  result <- liftIO . atomically $ createInviteLogic lookupUser addInvite user body

  case result of
    Left (status, msg) -> sendErrorAndFinish status msg
    Right invite -> S.json invite

data LookupCriteria = ById UserId | ByUsername T.Text

createInviteLogic :: Monad m =>
  (LookupCriteria -> m (Maybe User)) ->
  (Invite -> m AS.Id) ->
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
        invite = Invite { _inviteId = Nothing, _player1 = userId', _player2 = otherUserId }

      invId <- E.lift $ addInvite invite

      return $ invite & AS.inviteId .~ Just invId

data AcceptInviteBody = AcceptInviteBody {
  acceptInviteBodyInviteId :: AS.Id
} deriving (Eq, Show, Generic)

instance FromJSON AcceptInviteBody where

acceptInvite :: ActionM User -> TVar AppState -> ActionM ()
acceptInvite auth appStateTVar = do
  user <- auth
  body <- S.jsonData

  let
    lookupInvite :: AS.Id -> STM (Maybe AS.Invite)
    lookupInvite invId = view (invites . at invId) <$> readTVar appStateTVar

    removeInvite :: AS.Id -> STM ()
    removeInvite invId = modifyTVar' appStateTVar $ AS.deleteInvite invId

    addGame :: AS.UserId -> AS.UserId -> STM AS.GameAppState
    addGame uid otherUid = stateTVar appStateTVar $ AS.createGame uid otherUid

  result <- liftIO . atomically $ acceptInviteLogic lookupInvite removeInvite addGame user body

  case result of
    Left (status, errMsg) -> do
      S.status status
      S.json $ object ["error" .= errMsg]
    Right r -> S.json r

acceptInviteLogic :: Monad m =>
  (AS.Id -> m (Maybe Invite)) ->
  (AS.Id -> m ()) ->
  (AS.UserId -> AS.UserId -> m GameAppState) ->
  User ->
  AcceptInviteBody ->
  m (Either (Status, T.Text) GameAppState)
acceptInviteLogic lookupInvite removeInvite addGame user body = E.runExceptT $ do
  mInvite <- E.lift . lookupInvite $ acceptInviteBodyInviteId body
  inv <- E.liftEither $ V.noteE (status404, "No such invite") mInvite

  E.liftEither $ V.noteE (status403, "User not recipient of invite") $ guard (inv^.player2 == user^.userId)

  E.lift $ removeInvite (inv^?!inviteId._Just)
  gas <- E.lift $ addGame (inv^.player1) (inv^.player2)

  return gas
