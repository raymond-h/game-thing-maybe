{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module Auth0Management where

import GHC.Generics
import Data.Aeson
import Control.Lens hiding ((.=))
import Control.Concurrent.MVar
import qualified Data.Text as T
import qualified Data.ByteString.Char8 as BS
import qualified Network.Wreq as WR

modifyWreqOptions token opts =
  opts &~ do
    WR.auth ?= WR.oauth2Bearer (BS.pack . T.unpack $ token)

baseApiUrl auth0Config = "https://" <> (T.unpack $ domain auth0Config) <> "/api/v2"

get url auth0Config = withToken auth0Config $ \token -> do
  origRes <- WR.getWith (modifyWreqOptions token WR.defaults) ((baseApiUrl auth0Config) <> url)
  res <- WR.asJSON origRes
  return $ res ^. WR.responseBody

data GetTokenResponse = GetTokenResponse {
  access_token :: T.Text,
  expires_in :: Int
} deriving (Eq, Show, Generic)

instance FromJSON GetTokenResponse where

getToken domain clientId clientSecret = do
  let body = object [
          "grant_type" .= String "client_credentials",
          "client_id" .= String clientId,
          "client_secret" .= String clientSecret,
          "audience" .= (String $ "https://" <> domain <> "/api/v2/")
        ]

  res <- WR.asJSON =<< WR.post ("https://" <> (T.unpack domain) <> "/oauth/token") body

  let response = (res ^. WR.responseBody :: GetTokenResponse)

  return $ access_token response

data Auth0Config = Auth0Config {
  domain :: T.Text,
  clientId :: T.Text,
  clientSecret :: T.Text,
  accessTokenMVar :: MVar T.Text
} deriving (Eq)

getConfig domain clientId clientSecret = do
  accessToken <- getToken domain clientId clientSecret
  accessTokenMVar <- newMVar accessToken

  return $ Auth0Config {
    domain = domain,
    clientId = clientId,
    clientSecret = clientSecret,
    accessTokenMVar = accessTokenMVar
  }

withToken :: Auth0Config -> (T.Text -> IO a) -> IO a
withToken auth0Config f = do
  accessToken <- takeMVar (accessTokenMVar auth0Config)
  putMVar (accessTokenMVar auth0Config) accessToken

  f accessToken

data Auth0UserInfo = Auth0UserInfo {
  name :: T.Text,
  nickname :: T.Text
} deriving (Eq, Show, Generic)

instance FromJSON Auth0UserInfo where

getUserInfo :: BS.ByteString -> Auth0Config -> IO Auth0UserInfo
getUserInfo userId = get $ "/users/" <> (BS.unpack $ userId)
