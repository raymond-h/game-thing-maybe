{-# LANGUAGE TemplateHaskell #-}

module AppState where

import Control.Lens
import qualified Data.Map as M

data AppState = AppState {
  _users :: M.Map Int ()
} deriving (Eq, Show)

makeLenses ''AppState
