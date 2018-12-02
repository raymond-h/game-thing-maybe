{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Lib where

import Data.Aeson
import Network.Wai (Application)
import Network.Wai.Middleware.Routed
import System.Environment
import Crypto.JWT
import Data.Set (fromList)
import Data.Maybe
import Text.Read (readMaybe)
import Control.Lens hiding ((.=))
import Control.Monad.Trans (liftIO)
import qualified Data.Text as T
import qualified Web.Scotty as S

import Auth

getEnv' :: Read r => String -> IO (Maybe r)
getEnv' var = do
  mVal <- lookupEnv var
  return $ mVal >>= readMaybe

runApp :: IO ()
runApp = do
  port <- fromMaybe 8080 <$> getEnv' "PORT"

  (Just audience) <- preview stringOrUri <$> getEnv "JWT_AUDIENCE"
  (Just issuer) <- preview stringOrUri <$> getEnv "JWT_ISSUER"

  (Right jwkSet) <- fetchJWKSet =<< getEnv "AUTH0_DOMAIN"

  let
    jwtValidationSettings =
      defaultJWTValidationSettings (==audience)
        & validationSettingsAlgorithms .~ fromList [RS256]
        & jwtValidationSettingsIssuerPredicate .~ (==issuer)

  S.scotty port $ do
    S.get "/" $ do
      S.text "hello"

    S.get "/some-json" $ do
      S.json $ object ["foo" .= Number 23, "bar" .= Number 42]

    S.get "/.env" $ do
      verifyJWT jwtValidationSettings jwkSet

      env <- liftIO getEnvironment
      S.json . object . map (\(k, v) -> (T.pack k) .= v) $ env

    S.get "/auth" $ do
      verifyJWT jwtValidationSettings jwkSet

      S.text "gj on the authenticating"
