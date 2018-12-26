{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
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
import Util (stateTVar, sendErrorAndFinish)

newtype UserInfoBody = UserInfoBody {
  displayName :: Maybe T.Text
} deriving (Eq, Show, Generic)

instance FromJSON UserInfoBody where
instance ToJSON UserInfoBody where

isAlphanumeric :: T.Text -> Bool
isAlphanumeric = T.all alphanum
  where
    alphanum c = c `elem` ['A'..'Z'] ++ ['a'..'z'] ++ ['0'..'9']

validateUserInfoBody :: UserInfoBody -> Validation [(T.Text, T.Text)] UserInfoBody
validateUserInfoBody userInfo = displayNameValidation $> userInfo
  where
    displayNameValidation = case displayName userInfo of
      Nothing -> pure ()
      Just un ->
        (T.length un >= 2) `check` ("displayName", "Display name too short") *>
        (T.length un <= 10) `check` ("displayName", "Display name too long") *>
        isAlphanumeric un `check` ("displayName", "Display name must only contain alphanumerical symbols")

jsonInput valFn = handleValidation . valFn =<< S.jsonData

getUserInfo :: ActionM User -> TVar AppState -> ActionM ()
getUserInfo auth appStateTVar = do
  userId <- view userId <$> auth
  mUser <- view (userById userId) <$> (liftIO . readTVarIO) appStateTVar

  case mUser of
    Nothing -> sendErrorAndFinish status404 ("User '" <> userId <> "' does not exist")

    Just user -> S.json $ UserInfoBody { displayName = user ^. username }

updateUserInfo :: ActionM User -> TVar AppState -> ActionM ()
updateUserInfo auth appStateTVar = do
  userId' <- view userId <$> auth
  userInfo <- jsonInput validateUserInfoBody

  mUser <- liftIO . atomically . stateTVar appStateTVar . runState $ do
    forM_ (displayName userInfo) $ \newUsername ->
      (userById userId' . _Just . username) `assign` Just newUsername

    use $ userById userId'

  case (mUser :: Maybe User) of
    Nothing -> sendErrorAndFinish status404 ("User '" <> userId' <> "' does not exist" :: T.Text)

    Just user -> S.json $ UserInfoBody { displayName = user ^. username }
