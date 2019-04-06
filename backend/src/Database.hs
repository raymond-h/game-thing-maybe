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

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
User json
  Id T.Text
  username T.Text Maybe
  deriving Eq Show

Invite json
  player1 UserId
  player2 UserId
  deriving Eq Show

GameAppState json
  player1 UserId
  player2 UserId
  state T.Text
  deriving Eq Show
|]

runDbPool :: (MonadIO m, IsSqlBackend backend) => Pool backend -> ReaderT backend IO a -> m a
runDbPool pool query = liftIO $ flip runSqlPool pool $ query

fromDbUser :: Entity User -> AS.User
fromDbUser eUser = AS.User { AS._userId = (unUserKey $ entityKey eUser), AS._username = (userUsername $ entityVal eUser) }

toDbUser :: AS.User -> Entity User
toDbUser asUser = Entity (UserKey $ AS._userId asUser) $ User { userUsername = AS._username asUser }
