{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Auth where

import Control.Monad
import Control.Monad.Except
import Crypto.JWT
import Data.Maybe
import Data.Either.Combinators (mapLeft)
import Data.Time.Clock
import Control.Lens hiding ((.=))
import Data.Aeson
import Data.Aeson.Lens
import Data.Text as T
import Data.Text.Lazy as LT
import Data.Text.Encoding
import Network.HTTP.Types
import qualified Data.ByteString               as BS
import qualified Data.ByteString.Lazy          as LBS
import qualified Network.HTTP.Types            as H
import qualified Network.Wai                   as W
import qualified Network.Wreq                  as WR
import qualified Data.ByteString.Base16        as B16
import qualified Data.ByteString.Base64.URL    as B64
import qualified Web.Scotty as S
import qualified Web.Scotty.Trans as ST

import Validation (noteE)

verifyJWT :: JWKStore jwk => JWTValidationSettings -> jwk -> S.ActionM (BS.ByteString, ClaimsSet)
verifyJWT jwtValidationSettings jwkSet = do
  req <- S.request
  now <- liftIO getCurrentTime

  case verifyJWTPure jwtValidationSettings jwkSet req now of
    Left err -> do
      S.status status401
      S.json (object ["error" .= err])
      S.finish

    Right res ->
      return res

verifyJWTPure :: JWKStore jwk => JWTValidationSettings -> jwk -> W.Request -> UTCTime -> Either LT.Text (BS.ByteString, ClaimsSet)
verifyJWTPure jwtValidationSettings jwkSet req now = do
  token <- noteE "No bearer token" $ getAccessToken req

  claims <- mapLeft (LT.pack . show) $ verifyToken jwtValidationSettings now jwkSet token

  return (token, claims)

fetchJWKSet :: String -> IO (Either String JWKSet)
fetchJWKSet auth0Domain = do
  let url = "https://" <> auth0Domain <> "/.well-known/jwks.json"

  res <- WR.asJSON =<< WR.get url

  let jwkSetRes = fromJSON $ auth0JwkWorkaround $ res ^. WR.responseBody

  case jwkSetRes of
    Error   err    -> return $ Left err
    Success jwkSet -> return $ Right jwkSet

getAccessToken :: W.Request -> Maybe BS.ByteString
getAccessToken req = do
  authHeader <- lookup "Authorization" $ W.requestHeaders req

  BS.stripPrefix "Bearer " authHeader

verifyToken :: JWKStore jwk => JWTValidationSettings -> UTCTime -> jwk -> BS.ByteString -> Either JWTError ClaimsSet
verifyToken jwtValidationSettings now keyStore token = do
  jwt <- decodeCompact $ LBS.fromStrict token

  verifyClaimsAt jwtValidationSettings keyStore now jwt

auth0JwkWorkaround :: Value -> Value
auth0JwkWorkaround = over x5tInEveryKey fixProp
 where
  x5tInEveryKey = (key "keys") . values . (key "x5t") . _String
  fixProp =
    decodeUtf8 . B64.encode . fst . B16.decode . B64.decodeLenient . encodeUtf8
