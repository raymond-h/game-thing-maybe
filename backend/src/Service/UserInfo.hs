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

import AppState as AS
import Validation as V
import Util (Updater, stateTVar)

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

updateUserInfo :: ActionM User -> TVar AppState -> ActionM ()
updateUserInfo auth appStateTVar = do
  user <- auth
  body <- S.jsonData

  result <- liftIO . atomically $ updateUserInfoLogic user body (stateTVar appStateTVar)

  case result of
    Left (status, e) -> do
      S.status status
      S.json e
      S.finish
    Right r -> S.json r

-- precondition: "user" comes from "auth" function
updateUserInfoLogic :: Monad m =>
    User ->
    UserInfoBody ->
    Updater m AppState (Maybe User) ->
    m (Either (Status, M.Map T.Text [T.Text]) UserInfoBody)
updateUserInfoLogic user body modifyAppState = E.runExceptT $ do
  userInfo <- E.liftEither . first ((,) status400) . V.handleValidationE . validateUserInfoBody $ body

  let userId' = view userId user

  -- because "user" is a parameter, we already know the user has to already exist
  (Just newUser) <- E.lift . modifyAppState . runState $ do
    forM_ (userInfoUsername userInfo) $ \newUsername ->
      (userById userId' . _Just . username) `assign` Just newUsername

    use $ userById userId'

  return $ UserInfoBody { userInfoUsername = newUser ^. username }
