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

import qualified Network.Pusher as P

import Database.Persist as Ps
import Database.Persist.Sqlite as Ps
import Data.Pool
import qualified Database as DB

import AppState as AS
import qualified Validation as V
import qualified Game as G
import AuthUtil (requireUsernameE)
import Util (stateTVar, sendErrorAndFinish, guardError, pusherizedUserId)
import PusherCommon

data InviteBody =
  InviteBodyUserId UserId |
  InviteBodyUsername T.Text
  deriving (Eq, Show)

instance FromJSON InviteBody where
  parseJSON = withObject "InviteBody with username or user ID" $ \v -> asum [
      InviteBodyUserId <$> v .: "userId",
      InviteBodyUsername <$> v .: "username"
    ]

getInvites :: ActionM User -> Pool SqlBackend -> ActionM ()
getInvites auth dbPool = do
  user <- auth

  let
    lookupInvites uid = do
      let userKey = DB.UserKey uid
      dbInvs <- DB.runDbPool dbPool $ Ps.selectList ([DB.InvitePlayer1 ==. userKey] ||. [DB.InvitePlayer2 ==. userKey]) []
      return $ map DB.fromDbInviteEntity dbInvs

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

createInvite :: ActionM User -> Pool SqlBackend -> ([P.Channel] -> P.Event -> P.EventData -> S.ActionM ()) -> ActionM ()
createInvite auth dbPool pushClient = do
  user <- auth
  body <- S.jsonData

  let
    lookupUser (ById userId') = do
      mDbUser <- DB.runDbPool dbPool $ Ps.getEntity (DB.UserKey userId')
      return $ (DB.fromDbUser <$> mDbUser)

    lookupUser (ByUsername uname) = do
      mDbUser <- DB.runDbPool dbPool $ Ps.selectFirst [DB.UserUsername ==. Just uname] []
      return $ (DB.fromDbUser <$> mDbUser)

    addInvite inv = do
      invKey <- DB.runDbPool dbPool $ Ps.insert (DB.toDbInvite inv)
      return $ fromIntegral $ Ps.fromSqlKey invKey

  result <- createInviteLogic lookupUser addInvite (pushClient . fmap toChannel) user body

  case result of
    Left (status, msg) -> sendErrorAndFinish status msg
    Right invite -> S.json invite

data LookupCriteria = ById UserId | ByUsername T.Text

createInviteLogic :: Monad m =>
  (LookupCriteria -> m (Maybe User)) ->
  (Invite -> m AS.Id) ->
  ([EventChannel] -> P.Event -> P.EventData -> m ()) ->
  User ->
  InviteBody ->
  m (Either (Status, T.Text) Invite)
createInviteLogic lookupUser addInvite pushClient user body = E.runExceptT $ do
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
        invite = Invite { _inviteId = Nothing, _invitePlayer1 = userId', _invitePlayer2 = otherUserId }

      E.liftEither $ V.noteE (status400, "Cannot invite yourself") $ guard (userId' /= otherUserId)

      invId <- E.lift $ addInvite invite

      E.lift $ pushClient [Invites userId', Invites otherUserId] "update-invites" ""

      return $ invite & AS.inviteId .~ Just invId

data AcceptInviteBody = AcceptInviteBody {
  acceptInviteBodyInviteId :: AS.Id
} deriving (Eq, Show)

instance FromJSON AcceptInviteBody where
  parseJSON = withObject "AcceptInviteBody" $ \v ->
    AcceptInviteBody
      <$> v .: "id"

acceptInvite :: ActionM User -> Pool SqlBackend -> ([P.Channel] -> P.Event -> P.EventData -> S.ActionM ()) -> ActionM ()
acceptInvite auth dbPool pushClient = do
  user <- auth
  body <- S.jsonData

  let
    lookupInvite :: AS.Id -> S.ActionM (Maybe AS.Invite)
    lookupInvite invId = do
      mDbInvite <- DB.runDbPool dbPool $ Ps.getEntity (Ps.toSqlKey . fromIntegral $ invId)
      return $ DB.fromDbInviteEntity <$> mDbInvite

    removeInvite :: AS.Id -> S.ActionM ()
    removeInvite invId = DB.runDbPool dbPool $ Ps.delete invKey
      where
        invKey :: Ps.Key DB.Invite
        invKey = Ps.toSqlKey $ fromIntegral $ invId

    addGame :: AS.UserId -> AS.UserId -> S.ActionM AS.GameAppState
    addGame uid otherUid = do
      let
        userKey = DB.UserKey uid
        otherUserKey = DB.UserKey otherUid
      dbGasEntity <- DB.runDbPool dbPool $ Ps.insertEntity (DB.GameAppState userKey otherUserKey G.initialState)
      return $ DB.fromDbGameAppState dbGasEntity

  result <- acceptInviteLogic lookupInvite removeInvite addGame (pushClient . fmap toChannel) user body

  case result of
    Left (status, errMsg) -> do
      S.status status
      S.json $ object ["error" .= errMsg]
    Right r -> S.json r

acceptInviteLogic :: Monad m =>
  (AS.Id -> m (Maybe Invite)) ->
  (AS.Id -> m ()) ->
  (AS.UserId -> AS.UserId -> m GameAppState) ->
  ([EventChannel] -> P.Event -> P.EventData -> m ()) ->
  User ->
  AcceptInviteBody ->
  m (Either (Status, T.Text) GameAppState)
acceptInviteLogic lookupInvite removeInvite addGame pushClient user body = E.runExceptT $ do
  mInvite <- E.lift . lookupInvite $ acceptInviteBodyInviteId body
  inv <- E.liftEither $ V.noteE (status404, "No such invite") mInvite

  E.liftEither $ V.noteE (status403, "User not recipient of invite") $ guard (inv^.invitePlayer2 == user^.userId)

  E.lift $ removeInvite (inv^?!inviteId._Just)
  gas <- E.lift $ addGame (inv^.invitePlayer1) (inv^.invitePlayer2)

  E.lift $ pushClient [Invites (inv^.invitePlayer1), Invites (inv^.invitePlayer2)] "update-invites" ""

  return gas
