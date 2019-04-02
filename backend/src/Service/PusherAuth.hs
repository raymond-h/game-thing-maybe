{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DeriveGeneric #-}

module Service.PusherAuth where

import Data.Aeson
import Network.HTTP.Types
import Control.Lens hiding ((.=))
import Control.Monad (guard)
import Control.Monad.Trans (liftIO)
-- import qualified Data.Set as Set
import qualified Data.Text.Encoding as TE
import qualified Network.Pusher as P
import qualified Web.Scotty as S

import qualified AppState as AS
import Util (pusherizedUserId)

data PusherAuthBody = PusherAuthBody {
  pabSocketId :: P.SocketID,
  pabChannel :: P.Channel
} deriving (Eq, Show)

instance FromJSON PusherAuthBody where
  parseJSON = withObject "PusherAuthBody" $ \v ->
    PusherAuthBody
      <$> v .: "socket_id"
      <*> v .: "channel_name"

data PusherAuthResponse = PusherAuthResponse {
  paResAuthToken :: P.AuthSignature
} deriving (Eq, Show)

instance ToJSON PusherAuthResponse where
  toJSON res = object ["auth" .= (TE.decodeUtf8 $ paResAuthToken res)]

allowedChannelsForUserId :: AS.UserId -> [P.Channel]
allowedChannelsForUserId uid = [P.Channel P.Private (pusherizedUserId uid <> "-user-info")]

pusherAuthenticateLogic :: P.Credentials -> AS.User -> P.SocketID -> P.Channel -> Maybe PusherAuthResponse
pusherAuthenticateLogic creds user socketId channel = do
  guard $ channel `elem` allowedChannelsForUserId (user ^. AS.userId)

  case channel of
    P.Channel P.Presence _ -> Just . PusherAuthResponse $ P.authenticatePresence creds socketId channel ()
    P.Channel P.Private _ -> Just . PusherAuthResponse $ P.authenticatePrivate creds socketId channel
    _ -> Nothing

pusherAuthenticate :: S.ActionM AS.User -> P.Credentials -> S.ActionM ()
pusherAuthenticate auth creds = do
  user <- auth
  socketId <- S.param "socket_id"
  channel <- P.parseChannel <$> S.param "channel_name"

  liftIO $ putStrLn $ show (socketId, channel)

  case pusherAuthenticateLogic creds user socketId channel of
    Nothing -> do
      S.status forbidden403
      S.text "Forbidden"

    Just result -> S.json result
