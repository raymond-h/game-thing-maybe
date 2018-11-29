{-# LANGUAGE OverloadedStrings #-}

module Lib where

import           Data.Aeson
import           Network.Wai                    ( Application )
import qualified Web.Scotty                    as S

runApp :: IO ()
runApp = S.scotty 8080 $ do
  S.get "/" $ do
    S.text "hello"

  S.get "/some-json" $ do
    S.json $ object ["foo" .= Number 23, "bar" .= Number 42]
