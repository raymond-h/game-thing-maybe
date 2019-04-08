{-# LANGUAGE OverloadedStrings #-}

module PusherCommon where

import qualified Database.Persist.Sql as Ps
import qualified Data.Text as T
import qualified Network.Pusher as P

import qualified Database as DB
import qualified AppState as AS
import Util (pusherizedUserId)

data EventChannel =
  UserInfo AS.UserId
  | Invites AS.UserId
  | Game DB.GameAppStateId

toChannel :: EventChannel -> P.Channel
toChannel (UserInfo userId) = P.Channel P.Private (pusherizedUserId userId <> "-user-info")
toChannel (Invites userId) = P.Channel P.Private (pusherizedUserId userId <> "-invites")
toChannel (Game gameId) = P.Channel P.Public ("game-" <> (T.pack $ show $ Ps.fromSqlKey gameId))
