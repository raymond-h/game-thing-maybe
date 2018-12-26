{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Service.Invite where

import Control.Applicative
import Control.Concurrent.STM hiding (check)
import Control.Lens hiding ((.=))
import Control.Monad
import Control.Monad.Trans (liftIO)
import Data.Aeson
import Data.Foldable
import Data.Maybe
import Network.HTTP.Types
import Web.Scotty as S
import qualified Data.Text as T

import AppState as AS
import AuthUtil (requireUsername)
import Util (stateTVar, sendErrorAndFinish, guardError)

data InviteBody =
  InviteBodyUserId UserId |
  InviteBodyUsername T.Text
  deriving (Eq, Show)

instance FromJSON InviteBody where
  parseJSON = withObject "InviteBody with username or user ID" $ \v -> asum [
      InviteBodyUserId <$> v .: "userId",
      InviteBodyUsername <$> v .: "username"
    ]

getInvites :: ActionM User -> TVar AppState -> ActionM ()
getInvites auth appState = do
  userId <- view AS.userId <$> (requireUsername =<< auth)
  invites <- AS.findInvitesForUser userId <$> (liftIO . readTVarIO) appState
  S.json invites

createInvite :: ActionM User -> TVar AppState -> ActionM ()
createInvite auth appStateTVar = do
  user <- requireUsername =<< auth
  let userId' = user ^. userId

  let
    lensFrom (InviteBodyUserId uId) = userById uId
    lensFrom (InviteBodyUsername uName) = userByUsername uName

  otherUserLens <- lensFrom <$> S.jsonData

  join $ liftIO . atomically $ do
    appState <- readTVar appStateTVar

    case appState ^? otherUserLens . _Just . userId of
      Nothing -> return $ sendErrorAndFinish badRequest400 ("No such user" :: T.Text)

      Just otherUserId -> do
        let invite = Invite { _player1 = userId', _player2 = otherUserId }

        modifyTVar appStateTVar $ addInvite invite

        return $ S.json invite

