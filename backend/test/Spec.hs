{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE QuasiQuotes #-}

import Control.Lens
import Test.Hspec
import Network.HTTP.Types
import Test.Hspec.QuickCheck (prop)
import Test.Hspec.Wai
import Test.Hspec.Wai.JSON
import Test.QuickCheck
import Test.QuickCheck.Poly
import Data.Monoid
import Data.Foldable
import Data.Functor (($>))
import System.Environment
import Control.Concurrent.STM

import Lib (createApp, Environment(..))
import AppState as AS
import Util (adjustMatching)

waiExpectation :: WaiExpectation -> WaiExpectation
waiExpectation = id

setupEnv :: IO ()
setupEnv = do
  setEnv "JWT_AUDIENCE" "audience"
  setEnv "JWT_ISSUER" "issuer"
  setEnv "AUTH0_DOMAIN" "domain"
  setEnv "AUTH0_CLIENT_ID" "client-id"
  setEnv "AUTH0_CLIENT_SECRET" "client-secret"

resetAppState appStateTVar = atomically $ writeTVar appStateTVar testAppState

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

  appStateTVar <- runIO $ newTVarIO testAppState

  describe "REST app" $ beforeAll_ setupEnv $ before_ (resetAppState appStateTVar) $ with (createApp Test appStateTVar) $ do
    describe "/" $ do
      it "should work OK" $ do
        get "/" `shouldRespondWith` "hello"

    describe "/invites" $ do
      it "should refuse getting if no username set" $ do
        request methodGet "/invites" [("Authorization", "user1")] ""
          `shouldRespondWith`
          [json|{"error":"Must set username first"}|] { matchStatus = 401 }

      it "should refuse creating if no username set" $ waiExpectation $ do
        request methodPost "/invites" [("Authorization", "user1"), ("Content-Type", "application/json")] "{\"userId\":\"user2\"}"
          `shouldRespondWith`
          [json|{"error":"Must set username first"}|] { matchStatus = 401 }

      it "should get no invites by default" $ do
        request methodGet "/invites" [("Authorization", "user2")] ""
          `shouldRespondWith`
          [json|[]|] { matchStatus = 200 }

      it "should create invites by ID" $ waiExpectation $ do
        request methodPost "/invites" [("Authorization", "user2"), ("Content-Type", "application/json")] "{\"userId\":\"user1\"}"
          `shouldRespondWith`
          [json|{"player1":"user2","player2":"user1"}|] { matchStatus = 200 }

        request methodGet "/invites" [("Authorization", "user2")] ""
          `shouldRespondWith`
          [json|[{"player1":"user2","player2":"user1"}]|] { matchStatus = 200 }

      it "should create invites by username" $ waiExpectation $ do
        request methodPost "/invites" [("Authorization", "user2"), ("Content-Type", "application/json")] "{\"username\":\"anotheruser\"}"
          `shouldRespondWith`
          [json|{"player1":"user2","player2":"user3"}|] { matchStatus = 200 }

        request methodGet "/invites" [("Authorization", "user2")] ""
          `shouldRespondWith`
          [json|[{"player1":"user2","player2":"user3"}]|] { matchStatus = 200 }
