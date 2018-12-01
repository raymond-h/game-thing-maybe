{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Auth where

import Control.Monad
import Control.Monad.Except
import Crypto.JWT
import Data.Maybe
import Data.Time.Clock
import Control.Lens
import Data.Aeson
import Data.Aeson.Lens
import Data.Text.Lazy as LT
import Data.Text.Encoding
import qualified Data.ByteString               as BS
import qualified Data.ByteString.Lazy          as LBS
import qualified Network.HTTP.Types            as H
import qualified Network.Wai                   as W
import qualified Network.Wreq                  as WR
import qualified Data.ByteString.Base16        as B16
import qualified Data.ByteString.Base64.URL    as B64
import qualified Web.Scotty as S
import qualified Web.Scotty.Trans as ST

verifyJWT :: JWKStore jwk => JWTValidationSettings -> jwk -> S.ActionM ClaimsSet
verifyJWT jwtValidationSettings jwkSet = do
  req <- S.request
  case getAccessToken req of
    Nothing    -> S.raise $ ST.stringError "No bearer token"
    Just token -> do
      now    <- liftIO getCurrentTime

      case verifyToken jwtValidationSettings now jwkSet token of
        Left  err -> S.raise . ST.stringError . show $ err
        Right claims -> return claims

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
