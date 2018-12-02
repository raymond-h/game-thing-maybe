{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Lib where

import Data.Aeson
import Network.Wai (Application)
import Network.Wai.Middleware.Routed
import Network.Wai.Middleware.Cors
import System.Environment
import Crypto.JWT
import Data.Set (fromList)
import Data.Maybe
import Text.Read (readMaybe)
import Control.Lens hiding ((.=))
import Control.Monad.Trans (liftIO)
import qualified Data.ByteString.Char8 as BS
import qualified Data.Text as T
import qualified Web.Scotty as S

import Auth

getEnv' :: Read r => String -> IO (Maybe r)
getEnv' var = do
  mVal <- lookupEnv var
  return $ mVal >>= readMaybe

listOfPairsToObject :: [(String, String)] -> Value
listOfPairsToObject = object . (map $ uncurry (.=)) . over (mapped._1) T.pack

frontendCorsResourcePolicy frontendOrigin = simpleCorsResourcePolicy {
  corsOrigins = Just ([BS.pack frontendOrigin], True)
}

runApp :: IO ()
runApp = do
  port <- fromMaybe 8080 <$> getEnv' "PORT"
  isDev <- fromMaybe False <$> getEnv' "DEV"
  print ("Dev?", isDev)

  corsResPolicy <- frontendCorsResourcePolicy <$> getEnv "FRONTEND_ORIGIN"
  (Just audience) <- preview stringOrUri <$> getEnv "JWT_AUDIENCE"
  (Just issuer) <- preview stringOrUri <$> getEnv "JWT_ISSUER"

  (Right jwkSet) <- fetchJWKSet =<< getEnv "AUTH0_DOMAIN"

  let
    jwtValidationSettings =
      defaultJWTValidationSettings (==audience)
        & validationSettingsAlgorithms .~ fromList [RS256]
        & jwtValidationSettingsIssuerPredicate .~ (==issuer)

  S.scotty port $ do
    S.middleware $
      if isDev then simpleCors
        else cors $ (const . Just) corsResPolicy

    S.get "/" $ do
      S.text "hello"

    S.get "/some-json" $ do
      S.json $ object ["foo" .= Number 23, "bar" .= Number 42]

    S.get "/.env" $ do
      verifyJWT jwtValidationSettings jwkSet

      env <- liftIO getEnvironment
      S.json . listOfPairsToObject $ env

    S.get "/auth" $ do
      verifyJWT jwtValidationSettings jwkSet

      S.text "gj on the authenticating"
