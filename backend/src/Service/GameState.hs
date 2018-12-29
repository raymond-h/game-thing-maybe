module Service.GameState where

import Control.Concurrent.STM
import Web.Scotty as S

import AppState as AS

getGameState :: ActionM User -> TVar AppState -> ActionM ()
getGameState auth appStateTVar = do
  return ()

