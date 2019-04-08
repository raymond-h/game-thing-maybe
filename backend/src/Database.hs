{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Database where

import Data.Maybe (fromJust)
import Data.Pool
import Database.Persist
import Database.Persist.Sqlite
import Database.Persist.TH
import Control.Monad.Reader
import Control.Monad.Logger
import Control.Monad.IO.Class (liftIO)
import qualified Web.Scotty as S
import qualified Data.Text as T

import qualified AppState as AS
import qualified Game as G

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
User json
  Id T.Text
  username T.Text Maybe
  UniqueUsername username !force
  deriving Eq Show

Invite json
  player1 UserId
  player2 UserId
  deriving Eq Show

GameAppState json
  player1 UserId
  player2 UserId
  state G.State
  deriving Eq Show
|]

runDbPool :: (MonadIO m, IsSqlBackend backend) => Pool backend -> ReaderT backend IO a -> m a
runDbPool pool query = liftIO $ flip runSqlPool pool $ query

fromDbUser :: Entity User -> AS.User
fromDbUser eUser = AS.User { AS._userId = (unUserKey $ entityKey eUser), AS._userUsername = (userUsername $ entityVal eUser) }

toDbUser :: AS.User -> Entity User
toDbUser asUser = Entity (UserKey $ AS._userId asUser) $ User { userUsername = AS._userUsername asUser }

fromDbInviteEntity :: Entity Invite -> AS.Invite
fromDbInviteEntity eInvite = (fromDbInvite $ entityVal eInvite) {
    AS._inviteId = (Just $ fromIntegral $ fromSqlKey $ entityKey eInvite)
  }

toDbInviteEntity :: AS.Invite -> Entity Invite
toDbInviteEntity asInvite = Entity (toSqlKey $ fromIntegral $ fromJust $ AS._inviteId asInvite) $ toDbInvite asInvite

fromDbInvite :: Invite -> AS.Invite
fromDbInvite eInvite = AS.Invite {
    AS._inviteId = Nothing,
    AS._invitePlayer1 = (unUserKey $ invitePlayer1 $ eInvite),
    AS._invitePlayer2 = (unUserKey $ invitePlayer2 $ eInvite)
  }

toDbInvite :: AS.Invite -> Invite
toDbInvite asInvite =
  Invite {
    invitePlayer1 = UserKey $ AS._invitePlayer1 asInvite,
    invitePlayer2 = UserKey $ AS._invitePlayer2 asInvite
  }

fromDbGameAppState :: Entity GameAppState -> AS.GameAppState
fromDbGameAppState eGas = AS.GameAppState {
    AS._gameAppStateId = (fromIntegral $ fromSqlKey key),
    AS._gameAppStatePlayers = (unUserKey $ gameAppStatePlayer1 val, unUserKey $ gameAppStatePlayer2 val),
    AS._gameAppStateState = gameAppStateState val
  }
  where
    key = entityKey eGas
    val = entityVal eGas

toDbGameAppState :: AS.GameAppState -> Entity GameAppState
toDbGameAppState asGas = Entity (toSqlKey $ fromIntegral $ AS._gameAppStateId asGas) $
  GameAppState {
    gameAppStatePlayer1 = UserKey $ fst $ AS._gameAppStatePlayers asGas,
    gameAppStatePlayer2 = UserKey $ snd $ AS._gameAppStatePlayers asGas,
    gameAppStateState = AS._gameAppStateState asGas
  }

fromDbGameAppStateId :: Key GameAppState -> AS.GameId
fromDbGameAppStateId = fromIntegral . fromSqlKey

toDbGameAppStateId :: AS.GameId -> Key GameAppState
toDbGameAppStateId = toSqlKey . fromIntegral
