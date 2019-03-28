{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Service.UserInfo where

import GHC.Generics
import Data.Aeson
import Control.Lens hiding ((.=))
import Data.Either.Validation (Validation)
import Data.Maybe (fromJust, isJust)
import Data.Functor (($>))
import Control.Monad (forM_)
import Control.Monad.Trans (liftIO)
import Control.Concurrent.STM hiding (check)
import Control.Monad.State.Strict
import Network.HTTP.Types
import Web.Scotty as S
import qualified Data.Text as T

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

jsonInput valFn = handleValidation . valFn =<< S.jsonData

getUserInfo :: ActionM User -> ActionM ()
getUserInfo auth = S.json . getUserInfoLogic =<< auth

getUserInfoLogic :: User -> UserInfoBody
getUserInfoLogic user = UserInfoBody { userInfoUsername = user ^. username }

updateUserInfo :: ActionM User -> TVar AppState -> ActionM ()
updateUserInfo auth appStateTVar = do
  user <- auth

  body <- S.jsonData
  userInfo <- V.handleValidation $ validateUserInfoBody body

  result <- liftIO . atomically $ updateUserInfoLogic user userInfo (stateTVar appStateTVar)

  S.json result

-- precondition: "user" comes from "auth" function
updateUserInfoLogic :: Monad m =>
    User ->
    UserInfoBody ->
    Updater m AppState (Maybe User) ->
    m UserInfoBody
updateUserInfoLogic user userInfo modifyAppState = do
  let userId' = view userId user

  -- because "user" is a parameter, we already know the user has to already exist
  (Just newUser) <- modifyAppState . runState $ do
    forM_ (userInfoUsername userInfo) $ \newUsername ->
      (userById userId' . _Just . username) `assign` Just newUsername

    use $ userById userId'

  return $ UserInfoBody { userInfoUsername = newUser ^. username }
