{-# LANGUAGE OverloadedStrings #-}

module Lib where

import Data.Aeson
import Network.Wai (Application)
import Network.Wai.Middleware.Routed
import System.Environment
import Crypto.JWT
import Data.Set
import Control.Lens hiding ((.=))
import qualified Web.Scotty as S

import Auth

runApp :: IO ()
runApp = do
  (Just audience) <- preview stringOrUri <$> getEnv "JWT_AUDIENCE"
  (Just issuer) <- preview stringOrUri <$> getEnv "JWT_ISSUER"

  (Right jwkSet) <- fetchJWKSet =<< getEnv "AUTH0_DOMAIN"

  let
    jwtValidationSettings =
      defaultJWTValidationSettings (==audience)
        & validationSettingsAlgorithms .~ fromList [RS256]
        & jwtValidationSettingsIssuerPredicate .~ (==issuer)

  S.scotty 8080 $ do
    S.get "/" $ do
      S.text "hello"

    S.get "/some-json" $ do
      S.json $ object ["foo" .= Number 23, "bar" .= Number 42]

    S.get "/auth" $ do
      verifyJWT jwtValidationSettings jwkSet

      S.text "gj on the authenticating"
