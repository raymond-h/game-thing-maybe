{-# LANGUAGE OverloadedStrings #-}

module PusherCommon where

import qualified Network.Pusher as P

import qualified AppState as AS
import Util (pusherizedUserId)

data EventChannel =
  UserInfo AS.UserId
  | Invites AS.UserId

toChannel :: EventChannel -> P.Channel
toChannel (UserInfo userId) = P.Channel P.Private (pusherizedUserId userId <> "-user-info")
toChannel (Invites userId) = P.Channel P.Private (pusherizedUserId userId <> "-invites")
