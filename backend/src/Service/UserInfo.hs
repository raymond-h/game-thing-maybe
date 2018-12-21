{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Service.UserInfo where

import GHC.Generics
import Data.Aeson
import Data.Aeson.Lens
import Control.Lens hiding ((.=))
import Data.Either.Validation
import Data.Maybe (fromJust, isJust)
import Control.Monad.Trans (liftIO, MonadIO)
import Control.Concurrent.STM hiding (check)
import Control.Monad (guard)
import qualified Data.Text as T
import qualified Data.Map.Strict as M

import AppState (UserId)
import qualified AppState as AS
import Validation as V
import qualified Auth0Management as A0M
import Util (queryTVar, modifyTVarState)

data UserInfoBody = UserInfoBody {
  displayName :: Maybe T.Text
} deriving (Eq, Show, Generic)

instance FromJSON UserInfoBody where
instance ToJSON UserInfoBody where

isAlphanumeric :: T.Text -> Bool
isAlphanumeric = T.all alphanum
  where
    alphanum c = c `elem` ['A'..'Z'] ++ ['a'..'z'] ++ ['0'..'9']

validateUserInfoBody :: UserInfoBody -> Validation [(T.Text, T.Text)] UserInfoBody
validateUserInfoBody userInfo = displayNameValidation *> pure userInfo
  where
    displayNameValidation = case displayName userInfo of
      Nothing -> pure ()
      Just un ->
        (T.length un >= 2) `check` ("displayName", "Display name too short") *>
        (T.length un <= 10) `check` ("displayName", "Display name too long") *>
        (isAlphanumeric un) `check` ("displayName", "Display name must only contain alphanumerical symbols")

getUserInfo :: MonadIO m => TVar AS.AppState -> UserId -> m UserInfoBody
getUserInfo appState userId = do
  userProfile <- queryTVar appState $ fromJust . AS.getUserById userId

  return $ UserInfoBody {
    displayName = userProfile ^. AS.username
  }

updateUserInfo :: MonadIO m => TVar AS.AppState -> UserId -> UserInfoBody -> m UserInfoBody
updateUserInfo appState userId newUserInfo = do
  hasUser <- queryTVar appState $ AS.hasUser userId
  liftIO $ guard $ hasUser

  userProfile <- case displayName newUserInfo of
    Nothing -> queryTVar appState $ fromJust . AS.getUserById userId
    Just newDisplayName -> modifyTVarState appState $ AS.setUserUsername userId newDisplayName

  return $ UserInfoBody {
    displayName = userProfile ^. AS.username
  }
