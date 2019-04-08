{-# LANGUAGE OverloadedStrings #-}

module Service.GameState where

import Control.Concurrent.STM
import Web.Scotty as S
import Network.HTTP.Types
import qualified Control.Monad.Except as E
import qualified Data.Text as T

-- import AppState as AS
import Database.Persist as Ps
import Database.Persist.Sql as Ps
import Data.Pool
import qualified Database as DB
import qualified Validation as V

import Util (sendErrorAndFinish)

getGameState :: ActionM (Entity DB.User) -> Pool SqlBackend -> ActionM ()
getGameState _ dbPool = do
  gameId <- Ps.toSqlKey <$> S.param "gameId"

  mGame <- DB.runDbPool dbPool $ Ps.getEntity gameId

  let result = getGameStateLogic mGame

  case result of
    Left (status, err) -> sendErrorAndFinish status err
    Right gas -> S.json gas

getGameStateLogic ::
  Maybe (Entity DB.GameAppState) ->
  Either (Status, T.Text) (Entity DB.GameAppState)
getGameStateLogic = V.noteE (status404, "No such game")
