--{-# LANGUAGE EmptyDataDecls #-}
--{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module DatabaseExperiments where

import Data.Pool
import Database.Persist
import Database.Persist.Sqlite
import Database.Persist.TH
import Control.Monad.Reader
import Control.Monad.Logger
import Control.Monad.IO.Class (liftIO)
import qualified Web.Scotty as S
import qualified Data.Text.Lazy as T

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
Person
  name String
  age Int Maybe
  deriving Show
BlogPost
  title String
  authorId PersonId
  deriving Show
|]

-- databaseExperiment :: IO ()
-- databaseExperiment = runSqlite ":memory:" $ dbShenanigans

dbShenanigans :: ReaderT SqlBackend IO ()
dbShenanigans = do
    johnId <- insert $ Person "John Doe" $ Just 35
    janeId <- insert $ Person "Jane Doe" Nothing

    insert $ BlogPost "My fr1st p0st" johnId
    insert $ BlogPost "One more for good measure" johnId

    oneJohnPost <- selectList [BlogPostAuthorId ==. johnId] [LimitTo 1]
    liftIO $ print (oneJohnPost :: [Entity BlogPost])

    john <- get johnId
    liftIO $ print john

    delete janeId
    deleteWhere [BlogPostAuthorId ==. johnId]

runDbPool :: (MonadIO m, IsSqlBackend backend) => Pool backend -> ReaderT backend IO a -> m a
runDbPool pool query = liftIO $ flip runSqlPool pool $ query

scottyDbExperiment :: IO ()
scottyDbExperiment = runNoLoggingT $ withSqlitePool "local.db" 4 $ liftIO . runDbApp

runDbApp :: Pool SqlBackend -> IO ()
runDbApp pool = do
  runDbPool pool $ runMigration migrateAll
  runDbPool pool $ dbShenanigans

  S.scotty 8000 $ do
    S.get "/hi" $ do
      personList <- runDbPool pool $ selectList [] []
      S.text $ T.pack $ show (personList :: [Entity Person])

    S.post "/hi/:number" $ do
      num <- S.param "number"

      john <- runDbPool pool $ insertEntity $ Person "John Doe" $ Just num

      S.text $ T.pack $ show john
