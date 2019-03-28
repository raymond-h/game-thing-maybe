{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Validation where

import Network.HTTP.Types
import Data.Aeson (ToJSON, ToJSONKey, object, (.=))
import Web.Scotty
import Data.Bifunctor
import Data.Either.Validation
import Control.Monad
import qualified Data.Map.Strict as M
import qualified Data.Text.Lazy as LT

import Util (toMultiMap)

note :: e -> Maybe a -> Validation [e] a
note _ (Just a) = Success a
note e Nothing = Failure [e]

check :: Bool -> e -> Validation [e] ()
check bool n = note n $ guard bool

asValidation :: Either e a -> Validation [e] a
asValidation (Left e) = Failure [e]
asValidation (Right a) = Success a

fields :: Ord f => Validation [(f, e)] a -> Validation (M.Map f [e]) a
fields = first toMultiMap

maybeRespond :: ToJSON e => Validation e a -> ActionM a
maybeRespond ea = case ea of
  Failure e -> do
    status badRequest400
    json e
    finish

  Success a -> return a

maybeRespondE (Left e) = do
  status badRequest400
  json e
  finish
maybeRespondE (Right a) =
  return a

handleValidationE :: (Ord f, ToJSONKey f, ToJSON e) => Validation [(f, e)] a -> Either (M.Map f [e]) a
handleValidationE = validationToEither . fields

handleValidation :: (Ord f, ToJSONKey f, ToJSON e) => Validation [(f, e)] a -> ActionM a
handleValidation = maybeRespondE . first formatValError . handleValidationE
  where
    formatValError errMap = object ["errors" .= errMap]
