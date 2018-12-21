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
        let (user, appState) = ensureUser "hello" initialAppState

        appState ^. userById "hello" `shouldBe` Just user

      it "allows setting to Just user" $ do
        let
          appState = initialAppState
          newUser = User { _userId = "hello", _username = Just "Hello-Person" }
          updatedAppState = appState & userById "hello" .~ Just newUser

        _users updatedAppState `shouldBe` [newUser]

      it "allows setting to Nothing" $ do
        let
          (_, appState) = ensureUser "hello" initialAppState
          updatedAppState = appState & userById "hello" .~ Nothing

        _users updatedAppState `shouldBe` []

      it "allows modifying existing user" $ do
        let
          (_, appState) = ensureUser "hello" initialAppState
          updatedAppState = appState & userById "hello" . traverse . username .~ Just "Banana"

        appState ^. users . ix 0 . username `shouldBe` Nothing
        updatedAppState ^. users . ix 0 . username `shouldBe` Just "Banana"

