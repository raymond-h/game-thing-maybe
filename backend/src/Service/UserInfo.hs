{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Service.UserInfo where

import GHC.Generics
import Data.Aeson
import Data.Aeson.Text
import Control.Lens hiding ((.=))
import Data.Either.Validation (Validation)
import qualified Control.Monad.Except as E
import Data.Maybe (fromJust, isJust)
import Data.Functor (($>))
import Data.Bifunctor (first)
import Control.Monad (forM_)
import Control.Monad.Trans (liftIO)
import Control.Monad.State.Strict
import Network.HTTP.Types
import Web.Scotty as S
import qualified Data.Text as T
import qualified Data.Text.Lazy as LT
import qualified Data.Map.Strict as M

import qualified Network.Pusher as P

import Database.Persist as Ps
import Database.Persist.Sqlite as Ps
import Data.Pool
import qualified Database as DB

import AppState as AS
import Validation as V
import Util (Updater, pusherizedUserId, sendErrorAndFinish)
import PusherCommon

newtype UserInfoBody = UserInfoBody {
  userInfoUsername :: Maybe T.Text
} deriving (Eq, Show)

instance FromJSON UserInfoBody where
  parseJSON = withObject "user info body" $ \v ->
    UserInfoBody
      <$> v .:? "username"

instance ToJSON UserInfoBody where
  toJSON UserInfoBody{..} = object ["username" .= userInfoUsername]

isAlphanumeric :: T.Text -> Bool
isAlphanumeric = T.all alphanum
  where
    alphanum c = c `elem` ['A'..'Z'] ++ ['a'..'z'] ++ ['0'..'9']

validateUserInfoBody :: UserInfoBody -> Validation [(T.Text, T.Text)] UserInfoBody
validateUserInfoBody userInfo = usernameValidation $> userInfo
  where
    usernameValidation = case userInfoUsername userInfo of
      Nothing -> pure ()
      Just un ->
        (T.length un >= 2) `check` ("username", "Username too short") *>
        (T.length un <= 10) `check` ("username", "Username too long") *>
        isAlphanumeric un `check` ("username", "Username must only contain alphanumerical symbols")

getUserInfo :: ActionM User -> ActionM ()
getUserInfo auth = S.json . getUserInfoLogic =<< auth

getUserInfoLogic :: User -> UserInfoBody
getUserInfoLogic user = UserInfoBody { userInfoUsername = user ^. userUsername }

getSpecificUserInfo :: Pool SqlBackend -> ActionM ()
getSpecificUserInfo dbPool = do
  uid <- S.param "userId"

  let
    lookupUser :: AS.UserId -> S.ActionM (Maybe AS.User)
    lookupUser uid = (fmap . fmap) DB.fromDbUser $ DB.runDbPool dbPool $ Ps.getEntity (DB.UserKey uid)

  result <- getSpecificUserInfoLogic <$> lookupUser uid

  case result of
    Left (status, err) -> sendErrorAndFinish status err
    Right r -> S.json r

getSpecificUserInfoLogic :: Maybe AS.User -> Either (Status, T.Text) UserInfoBody
getSpecificUserInfoLogic Nothing = Left (status404, "No such user")
getSpecificUserInfoLogic (Just user) = Right $ getUserInfoLogic user

updateUserInfo :: ActionM User -> Pool SqlBackend -> ([P.Channel] -> P.Event -> P.EventData -> S.ActionM ()) -> ActionM ()
updateUserInfo auth dbPool pushClient = do
  user <- auth
  body <- S.jsonData
  let
    isUsernameInUse uname = do
      n <- DB.runDbPool dbPool $ Ps.count [DB.UserUsername ==. Just uname]
      return $ n > 0

    updateUser user =
      let
        dbUser = DB.toDbUser user
      in
        DB.runDbPool dbPool $ Ps.replace (Ps.entityKey dbUser) (Ps.entityVal dbUser)

  result <- updateUserInfoLogic isUsernameInUse updateUser (pushClient . fmap toChannel) user body

  case result of
    Left e -> do
      S.status badRequest400
      S.json $ object ["errors" .= e]
    Right r -> S.json r

updateUserInfoLogic :: Monad m =>
    (T.Text -> m Bool) ->
    (User -> m ()) ->
    ([EventChannel] -> P.Event -> P.EventData -> m ()) ->
    User ->
    UserInfoBody ->
    m (Either (M.Map T.Text [T.Text]) UserInfoBody)
updateUserInfoLogic isUsernameInUse updateUser pushClient user body = E.runExceptT $ do
  userInfo <- E.liftEither . V.handleValidationE . validateUserInfoBody $ body

  forM_ (userInfoUsername userInfo) $ \newUsername -> do
    unless (user ^. AS.userUsername == Just newUsername) $ do
      isInUse <- E.lift $ isUsernameInUse newUsername

      when isInUse $ E.throwError $ M.singleton "username" ["Username already in use"]

  let
    newUser = flip execState user $ do
      forM_ (userInfoUsername userInfo) $ \newUsername ->
        userUsername `assign` Just newUsername

  E.lift $ updateUser newUser

  let newUserInfo = UserInfoBody { userInfoUsername = newUser ^. userUsername }

  when (newUser /= user) $ do
    E.lift $ pushClient [UserInfo (user^.userId)] "update-user-info" $ LT.toStrict $ encodeToLazyText newUserInfo

  return newUserInfo
