{-# LANGUAGE OverloadedStrings #-}

import Control.Lens
import Test.Hspec
import Test.QuickCheck

import AppState as AS

main :: IO ()
main = hspec $ do
  describe "AppState" $ do
    describe "userById lens" $ do
      it "allows getting" $ do
        let (user, appState) = AS.ensureUser "hello" AS.initialAppState

        (appState ^? AS.userById "hello") `shouldBe` Just user

      it "allows setting" $ do
        let
          (_, appState) = AS.ensureUser "hello" AS.initialAppState
          newUser = User { _userId = "hello", _username = Just "Hello-Person" }
          updatedAppState = appState & AS.userById "hello" .~ newUser

        (updatedAppState ^? AS.userById "hello") `shouldBe` Just newUser

      it "allows updating" $ do
        let
          (_, appState) = AS.ensureUser "hello" AS.initialAppState
          newUser = User { _userId = "hello", _username = Just "Hello-Person" }
          updatedAppState = appState & AS.userById "hello" %~ (AS.username ?~ "Hello-Person")

        (updatedAppState ^? AS.userById "hello") `shouldBe` Just newUser
