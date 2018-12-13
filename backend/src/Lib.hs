{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Lib where

import Data.Aeson
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
import Control.Concurrent.STM
import qualified Data.ByteString.Char8 as BS
import qualified Data.Text as T
import qualified Data.Text.Lazy as LT
import qualified Web.Scotty as S

import Auth
import qualified AppState as AS

getEnv' :: Read r => String -> IO (Maybe r)
getEnv' var = do
  mVal <- lookupEnv var
  return $ mVal >>= readMaybe

listOfPairsToObject :: [(String, String)] -> Value
listOfPairsToObject = object . (map $ uncurry (.=)) . over (mapped._1) T.pack

frontendCorsResourcePolicy frontendOrigin = simpleCorsResourcePolicy {
  corsOrigins = Just ([BS.pack frontendOrigin], True)
}

devCorsResourcePolicy = simpleCorsResourcePolicy {
  corsOrigins = Nothing,
  corsMethods = simpleMethods,
  corsRequestHeaders = ["Authorization"]
}

stateTVar :: TVar s -> (s -> (a, s)) -> STM a
stateTVar var f = do
  s <- readTVar var
  let (a, s') = f s
  writeTVar var s'
  return a
{-# INLINE stateTVar #-}

atomically' :: MonadIO m => STM a -> m a
atomically' = liftIO . atomically

authenticate appState jwtValidationSettings jwkSet = do
  (_, claimsSet) <- verifyJWT jwtValidationSettings jwkSet

  let userId = T.pack $ view (claimSub._Just.string) claimsSet

  atomically' $ stateTVar appState (AS.ensureUser userId)

runApp :: IO ()
runApp = do
  port <- fromMaybe 8080 <$> getEnv' "PORT"
  isDev <- fromMaybe False <$> getEnv' "DEV"
  print ("Dev?", isDev)

  corsResPolicy <- frontendCorsResourcePolicy <$> getEnv "FRONTEND_ORIGIN"
  (Just audience) <- preview stringOrUri <$> getEnv "JWT_AUDIENCE"
  (Just issuer) <- preview stringOrUri <$> getEnv "JWT_ISSUER"

  (Right jwkSet) <- fetchJWKSet =<< getEnv "AUTH0_DOMAIN"

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

    S.middleware $ cors . const . Just $
      if isDev then devCorsResourcePolicy else corsResPolicy

    S.get "/" $ do
      S.text "hello"

    S.get "/some-json" $ auth >> do
      S.json $ object ["foo" .= Number 23, "bar" .= Number 42]

    S.get "/.env" $ auth >> do
      env <- liftIO getEnvironment
      S.json . listOfPairsToObject $ env

    S.get "/auth" $ do
      user <- auth
      S.text $ "gj on the authenticating: " <> (LT.pack $ show user)

    S.get "/me" $ auth >>= S.json

    S.get "/users" $ do
      appStateData <- liftIO $ readTVarIO appState
      S.text $ LT.pack $ show appStateData
