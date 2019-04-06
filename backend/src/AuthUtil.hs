{-# LANGUAGE OverloadedStrings #-}

module AuthUtil where

import Control.Lens
import Network.HTTP.Types
import Web.Scotty as S
import qualified Data.Text as T

import AppState as AS
import Util (guardError, sendErrorAndFinish)

requireUsername :: AS.User -> ActionM AS.User
requireUsername user =
  case requireUsernameE user of
    Left (status, msg) -> sendErrorAndFinish status msg
    Right user -> return user

requireUsernameE :: AS.User -> Either (Status, T.Text) AS.User
requireUsernameE user =
  if has (userUsername . _Just) user
    then Right user
    else Left (forbidden403, ("Must set username first" :: T.Text))
