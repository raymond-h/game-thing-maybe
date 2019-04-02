{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Service.UserInfo where

import GHC.Generics
import Data.Aeson
import Control.Lens hiding ((.=))
import Data.Either.Validation (Validation)
import qualified Control.Monad.Except as E
import Data.Maybe (fromJust, isJust)
import Data.Functor (($>))
import Data.Bifunctor (first)
import Control.Monad (forM_)
import Control.Monad.Trans (liftIO)
import Control.Concurrent.STM hiding (check)
import Control.Monad.State.Strict
import Network.HTTP.Types
import Web.Scotty as S
import qualified Data.Text as T
import qualified Data.Map.Strict as M

import qualified Network.Pusher as P

import AppState as AS
import Validation as V
import Util (Updater, stateTVar, pusherizedUserId)

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
getUserInfoLogic user = UserInfoBody { userInfoUsername = user ^. username }

updateUserInfo :: ActionM User -> TVar AppState -> ([P.Channel] -> P.Event -> P.EventData -> S.ActionM ()) -> ActionM ()
updateUserInfo auth appStateTVar pushClient = do
  user <- auth
  body <- S.jsonData
  let updateUser user = liftIO $ atomically $ modifyTVar' appStateTVar $ AS.updateUser user

  result <- updateUserInfoLogic updateUser pushClient user body

  case result of
    Left e -> do
      S.status badRequest400
      S.json $ object ["errors" .= e]
    Right r -> S.json r

updateUserInfoLogic :: Monad m =>
    (User -> m ()) ->
    ([P.Channel] -> P.Event -> P.EventData -> m ()) ->
    User ->
    UserInfoBody ->
    m (Either (M.Map T.Text [T.Text]) UserInfoBody)
updateUserInfoLogic updateUser pushClient user body = E.runExceptT $ do
  userInfo <- E.liftEither . V.handleValidationE . validateUserInfoBody $ body

  let
    newUser = flip execState user $ do
      forM_ (userInfoUsername userInfo) $ \newUsername ->
        username `assign` Just newUsername

  E.lift $ updateUser newUser

  when (newUser /= user) $ do
    let uid = pusherizedUserId (user^.userId)
    -- TODO: Send events only to private channel of user who did the changing
    E.lift $ pushClient [P.Channel P.Public (uid <> "-user-info")] "update-user-info" ""

  return $ UserInfoBody { userInfoUsername = newUser ^. username }
