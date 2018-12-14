{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module Auth0Management where

import GHC.Generics
import Data.Aeson
import Data.Aeson.Lens
import Control.Lens hiding ((.=))
import Control.Concurrent.MVar
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.ByteString.Char8 as BS
import Network.Wreq as W

baseApiUrl :: Auth0Config -> String
baseApiUrl auth0Config = "https://" <> (T.unpack $ domain auth0Config) <> "/api/v2"

fetchToken :: T.Text -> T.Text -> T.Text -> IO T.Text
fetchToken domain clientId clientSecret = do
  let body = object [
          "grant_type" .= String "client_credentials",
          "client_id" .= String clientId,
          "client_secret" .= String clientSecret,
          "audience" .= (String $ "https://" <> domain <> "/api/v2/")
        ]

  res <- asJSON =<< post ("https://" <> (T.unpack domain) <> "/oauth/token") body

  let accessToken = (res :: Response Value) ^. responseBody . key "access_token" . _String

  return accessToken

data Auth0Config = Auth0Config {
  domain :: T.Text,
  clientId :: T.Text,
  clientSecret :: T.Text,
  accessToken :: T.Text
} deriving (Eq)

getConfig :: T.Text -> T.Text -> T.Text -> IO Auth0Config
getConfig domain clientId clientSecret = do
  accessToken <- fetchToken domain clientId clientSecret

  return $ Auth0Config {
    domain = domain,
    clientId = clientId,
    clientSecret = clientSecret,
    accessToken = accessToken
  }

addAuth0 :: Auth0Config -> W.Options -> W.Options
addAuth0 auth0Config = auth .~ justBearer auth0Config
  where
    justBearer = Just . oauth2Bearer . T.encodeUtf8 . accessToken

type UserInfo = Value

-- data UserInfo = UserInfo {
--   name :: T.Text,
--   nickname :: T.Text
-- } deriving (Eq, Show, Generic)

-- instance FromJSON UserInfo where
-- instance ToJSON UserInfo where

getUserInfo :: T.Text -> Auth0Config -> IO UserInfo
getUserInfo userId auth0Config = do
  let opts = defaults & addAuth0 auth0Config

  res <- asJSON =<< getWith opts (baseApiUrl auth0Config <> "/users/" <> T.unpack userId)

  return $ res^.responseBody

listUsers :: Auth0Config -> IO [UserInfo]
listUsers auth0Config = do
  let opts = defaults & addAuth0 auth0Config

  res <- asJSON =<< getWith opts (baseApiUrl auth0Config <> "/users")

  return $ res^.responseBody

searchUsersV3 :: T.Text -> Auth0Config -> IO [UserInfo]
searchUsersV3 query auth0Config = do
  let opts = defaults
        & addAuth0 auth0Config
        & param "search_engine" .~ ["v3"]
        & param "q" .~ [query]

  res <- asJSON =<< getWith opts (baseApiUrl auth0Config <> "/users")

  return $ res^.responseBody
