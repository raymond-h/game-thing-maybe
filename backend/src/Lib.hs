{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DeriveGeneric #-}

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
import qualified Data.ByteString.Char8 as BS
import qualified Data.Map.Strict as M
import qualified Data.Text as T
import qualified Data.Text.Lazy as LT
import qualified Web.Scotty as S

import Auth
import Validation as V
import qualified AppState as AS
import qualified Auth0Management as A0M

getEnv' :: Read r => String -> IO (Maybe r)
getEnv' var = do
  mVal <- lookupEnv var
  return $ mVal >>= readMaybe

listOfPairsToObject :: [(String, String)] -> Value
listOfPairsToObject = object . (map $ uncurry (.=)) . over (mapped._1) T.pack

frontendCorsResourcePolicy frontendOrigin = simpleCorsResourcePolicy {
  corsOrigins = Just ([BS.pack frontendOrigin], True),
  corsRequestHeaders = ["Authorization"]
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

data UserInfoBody = UserInfoBody {
  displayName :: Maybe T.Text
} deriving (Eq, Show, Generic)

instance FromJSON UserInfoBody where
instance ToJSON UserInfoBody where

isAlphanumeric :: T.Text -> Bool
isAlphanumeric = T.all alphanum
  where
    alphanum c = c `elem` ['A'..'Z'] ++ ['a'..'z'] ++ ['0'..'9']

validateUserInfo :: UserInfoBody -> Validation [(T.Text, T.Text)] UserInfoBody
validateUserInfo userInfo = displayNameValidation *> pure userInfo
  where
    displayNameValidation = case displayName userInfo of
      Nothing -> pure ()
      Just un ->
        (T.length un >= 2) `check` ("displayName", "Display name too short") *>
        (T.length un <= 10) `check` ("displayName", "Display name too long") *>
        (isAlphanumeric un) `check` ("displayName", "Display name must only contain alphanumerical symbols")

runApp :: IO ()
runApp = do
  port <- fromMaybe 8080 <$> getEnv' "PORT"
  isDev <- fromMaybe False <$> getEnv' "DEV"
  print ("Dev?", isDev)

  corsResPolicy <- frontendCorsResourcePolicy <$> getEnv "FRONTEND_ORIGIN"
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
      userId <- auth
      S.text $ "gj on the authenticating: " <> (LT.pack $ show userId)

    S.get "/me" $ do
      userId <- auth
      S.json userId

    S.put "/user-info" $ do
      userId <- auth
      userInfo <- handleValidation . validateUserInfo =<< S.jsonData

      let newMetadata :: M.Map T.Text Value =
            M.empty
              & at "displayName" .~ (String <$> displayName userInfo)

      userProfile <- liftIO $ if (not $ M.null newMetadata)
        then A0M.updateUserInfo userId (object ["user_metadata" .= newMetadata]) a0Config
        else A0M.getUserInfo userId a0Config

      S.json $ object ["displayName" .= (userProfile ^. key "user_metadata" . key "displayName" . _String)]

    S.get "/users" $ do
      appStateData <- liftIO $ readTVarIO appState
      S.text $ LT.pack $ show appStateData
