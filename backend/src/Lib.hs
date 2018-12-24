{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Lib where

import GHC.Generics
import Data.Aeson
import Data.Aeson.Lens
import Network.Wai (Application)
import Network.Wai.Middleware.RequestLogger
import Network.Wai.Middleware.Cors
import System.Environment
import Crypto.JWT
import Data.Set (fromList)
import Data.Maybe
import Text.Read (readMaybe)
import Control.Lens hiding ((.=))
import Control.Monad.Trans (liftIO, MonadIO)
import Control.Concurrent.STM hiding (check)
import Control.Monad
import Data.Either.Validation
import Network.HTTP.Types
import qualified Data.ByteString.Char8 as BS
import qualified Data.Map.Strict as M
import qualified Data.Text as T
import qualified Data.Text.Lazy as LT
import qualified Web.Scotty as S

import Auth
import Validation as V
import Util (queryTVar, modifyTVarState)
import qualified AppState as AS
import qualified Auth0Management as A0M

import Service.UserInfo as UI
import Service.Invite as I

getEnv' :: Read r => String -> IO (Maybe r)
getEnv' var = do
  mVal <- lookupEnv var
  return $ mVal >>= readMaybe

listOfPairsToObject :: [(String, String)] -> Value
listOfPairsToObject = object . map (uncurry (.=)) . over (mapped._1) T.pack

frontendCorsResourcePolicy frontendOrigin = simpleCorsResourcePolicy {
  corsOrigins = Just ([BS.pack frontendOrigin], True),
  corsMethods = ["PUT"],
  corsRequestHeaders = ["Authorization", "Content-Type"]
}

devCorsResourcePolicy = simpleCorsResourcePolicy {
  corsOrigins = Nothing,
  corsMethods = ["PUT"],
  corsRequestHeaders = ["Authorization", "Content-Type"]
}

authenticate appState jwtValidationSettings jwkSet = do
  (_, claimsSet) <- verifyJWT jwtValidationSettings jwkSet

  let userId = T.pack $ view (claimSub._Just.string) claimsSet

  user <- modifyTVarState appState $ AS.ensureUser userId

  return (user :: AS.User)

checkError cond status errMsg = unless cond $ do
  S.status status
  S.json $ object ["error" .= errMsg]
  S.finish

runApp :: IO ()
runApp = do
  port <- fromMaybe 8080 <$> getEnv' "PORT"
  isDev <- fromMaybe False <$> getEnv' "DEV"
  print ("Dev?", isDev)

  corsResPolicy <- if isDev
    then return devCorsResourcePolicy
    else frontendCorsResourcePolicy <$> getEnv "FRONTEND_ORIGIN"

  (Just audience) <- preview stringOrUri <$> getEnv "JWT_AUDIENCE"
  (Just issuer) <- preview stringOrUri <$> getEnv "JWT_ISSUER"

  (Right jwkSet) <- fetchJWKSet =<< getEnv "AUTH0_DOMAIN"

  domain <- T.pack <$> getEnv "AUTH0_DOMAIN"
  clientId <- T.pack <$> getEnv "AUTH0_CLIENT_ID"
  clientSecret <- T.pack <$> getEnv "AUTH0_CLIENT_SECRET"
  a0Config <- A0M.getConfig domain clientId clientSecret

  appState <- newTVarIO AS.initialAppState

  let
    jwtValidationSettings =
      defaultJWTValidationSettings (==audience)
        & algorithms .~ fromList [RS256]
        & issuerPredicate .~ (==issuer)
        & allowedSkew .~ 30 * 60

    auth = authenticate appState jwtValidationSettings jwkSet

  S.scotty port $ do
    S.middleware $ if isDev then logStdoutDev else logStdout

    S.middleware . cors . const . Just $ corsResPolicy

    S.get "/" $ do
      S.text "hello"

    S.get "/some-json" $ auth >> do
      S.json $ object ["foo" .= Number 23, "bar" .= Number 42]

    S.get "/.env" $ auth >> do
      env <- liftIO getEnvironment
      S.json . listOfPairsToObject $ env

    S.get "/auth" $ do
      userId <- view AS.userId <$> auth
      S.text $ "gj on the authenticating: " <> LT.pack (show userId)

    S.get "/me" $ do
      userId <- view AS.userId <$> auth
      S.json userId

    S.get "/user-info" $ UI.getUserInfo auth appState
    S.put "/user-info" $ UI.updateUserInfo auth appState

    S.get "/invites" $ I.getInvites auth appState
    S.post "/invites" $ I.createInvite auth appState
