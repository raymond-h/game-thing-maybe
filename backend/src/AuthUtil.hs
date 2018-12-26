{-# LANGUAGE OverloadedStrings #-}

module AuthUtil where

import Control.Lens
import Network.HTTP.Types
import qualified Data.Text as T

import AppState as AS
import Util (guardError)

requireUsername user = do
  guardError unauthorized401 ("Must set username first" :: T.Text)
    $ has (username . _Just) user

  return user

