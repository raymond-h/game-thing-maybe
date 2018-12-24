{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

import Control.Lens
import Test.Hspec
import Test.Hspec.QuickCheck (prop)
import Test.QuickCheck
import Test.QuickCheck.Poly
import Data.Monoid
import Data.Foldable
import Data.Functor (($>))

import AppState as AS
import Util (adjustMatching)

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
          updatedAppState = appState & userById "hello" ?~ newUser

        _users updatedAppState `shouldBe` [newUser]

      it "allows setting to Nothing" $ do
        let
          (_, appState) = ensureUser "hello" initialAppState
          updatedAppState = appState & userById "hello" .~ Nothing

        _users updatedAppState `shouldBe` []

      it "allows modifying existing user" $ do
        let
          (_, appState) = ensureUser "hello" initialAppState
          updatedAppState = appState & userById "hello" . traverse . username ?~ "Banana"

        appState ^. users . ix 0 . username `shouldBe` Nothing
        updatedAppState ^. users . ix 0 . username `shouldBe` Just "Banana"

  describe "Util" $ do
    describe "adjustMatching" $ do
      it "can map first match" $ do
        adjustMatching (==3) (fmap (+10)) [1, 2, 3, 4, 3] `shouldBe` [1, 2, 13, 4, 3]

      it "can remove first match" $ do
        adjustMatching (==3) (\(Just 3) -> Nothing) [1, 2, 3, 4, 3] `shouldBe` [1, 2, 4, 3]

      prop "can add a new element if no matches" $
        \(x :: A) xs -> adjustMatching (const False) (\Nothing -> Just x) xs `shouldBe` xs ++ [x]

      prop "causes no difference if predicate matches no element and function returns Nothing" $
        \(xs :: [A]) -> adjustMatching (const False) (\Nothing -> Nothing) xs `shouldBe` xs

