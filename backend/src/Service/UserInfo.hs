{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Service.UserInfo where

import GHC.Generics
import Data.Aeson
import Data.Aeson.Lens
import Control.Lens hiding ((.=))
import Data.Either.Validation
import qualified Data.Text as T
import qualified Data.Map.Strict as M

import AppState (UserId)
import Validation as V
import qualified Auth0Management as A0M

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

getUserInfo :: A0M.Auth0Config -> UserId -> IO UserInfoBody
getUserInfo a0Config userId = do
  userProfile <- A0M.getUserInfo userId a0Config

  return $ UserInfoBody {
    displayName = (userProfile ^? key "user_metadata" . key "displayName" . _String)
  }

updateUserInfo :: A0M.Auth0Config -> UserId -> UserInfoBody -> IO UserInfoBody
updateUserInfo a0Config userId newUserInfo = do
  let
    newMetadata :: M.Map T.Text Value
    newMetadata =
        M.empty
          & at "displayName" .~ (String <$> displayName newUserInfo)

  userProfile <- if (not $ M.null newMetadata)
    then A0M.updateUserInfo userId (object ["user_metadata" .= newMetadata]) a0Config
    else A0M.getUserInfo userId a0Config

  return $ UserInfoBody {
    displayName = (userProfile ^? key "user_metadata" . key "displayName" . _String)
  }
