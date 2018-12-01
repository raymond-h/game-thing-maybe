{-# LANGUAGE OverloadedStrings #-}

module Lib where

import Data.Aeson
import Network.Wai (Application)
import Network.Wai.Middleware.Routed
import System.Environment
import Crypto.JWT
import Control.Lens (preview)
import qualified Web.Scotty as S

import Auth

runApp :: IO ()
runApp = do
  (Just audience) <- preview stringOrUri <$> getEnv "JWT_AUDIENCE"
  (Right jwkSet) <- fetchJWKSet =<< getEnv "AUTH0_DOMAIN"

  let
    audCheck = (==audience)
    jwtValidationSettings = defaultJWTValidationSettings audCheck

  S.scotty 8080 $ do
    S.get "/" $ do
      S.text "hello"

    S.get "/some-json" $ do
      S.json $ object ["foo" .= Number 23, "bar" .= Number 42]

    S.get "/auth" $ do
      verifyJWT jwtValidationSettings jwkSet

      S.text "gj on the authenticating"
