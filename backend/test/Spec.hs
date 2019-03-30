{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE QuasiQuotes #-}

import Data.Maybe (fromJust, isJust)
import Data.Either (isLeft)
import Control.Lens hiding ((.=))
import Data.Aeson hiding (json)
import qualified Control.Monad.State.Strict as S
import Test.Hspec as H
import Network.HTTP.Types
import Test.Hspec.QuickCheck (prop)
import Test.Hspec.Wai
import Test.Hspec.Wai.JSON
import qualified Test.Hspec.Wai.QuickCheck as WQC
import Test.QuickCheck
import Test.QuickCheck.Poly
import Data.Monoid
import Data.Foldable
import Data.Functor (($>))
import System.Environment
import Control.Concurrent.STM
import qualified Data.Text as T

import Lib (createApp, Environment(..))
import AppState as AS
import qualified Service.UserInfo as UI
import qualified Service.Invite as I
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

runInState = flip S.runState

resetAppState appStateTVar = atomically $ writeTVar appStateTVar testAppState

main :: IO ()
main = hspec $ do
  describe "AppState" $ do
    describe "nextId" $ do
      prop "should always return a value greater than all input values" $
        \(NonEmpty xs) -> all (\x -> x < nextId xs) xs

      prop "should equal the successor to at least one input value" $
        \(NonEmpty nnXs) ->
          let xs = map getNonNegative nnXs
          in any (\x -> succ x == nextId xs) xs

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

  describe "App logic" $ do
    describe "user info service" $ do
      let
        updateUser :: User -> S.State AS.AppState ()
        updateUser = S.modify . AS.updateUser

      it "can get user info" $ do
        let
          user = User { _userId = "whatever", _username = Just "cool-username" }

        UI.getUserInfoLogic user `shouldBe` UI.UserInfoBody (Just "cool-username")

      it "can set username" $ do
        let
          user = User { _userId = "hello", _username = Nothing }
          startAppState = addUser user initialAppState

          (result, endAppState) = runInState startAppState $
            UI.updateUserInfoLogic updateUser user (UI.UserInfoBody $ Just "username")

        endAppState ^? (userById "hello")._Just.username._Just `shouldBe` Just "username"
        result `shouldBe` (Right $ UI.UserInfoBody (Just "username"))

      let
        usernameValidationTest un = do
          let
            user = User { _userId = "hello", _username = Nothing }
            startAppState = addUser user initialAppState

            (result, endAppState) = runInState startAppState $
              UI.updateUserInfoLogic updateUser user (UI.UserInfoBody $ Just un)

          endAppState ^? (userById "hello")._Just.username._Just `shouldBe` Nothing
          result ^? _Left.(at "username") `shouldSatisfy` isJust

      it "disallows too long username" $ usernameValidationTest "veryveryverylongusername"
      it "disallows too short username" $ usernameValidationTest "u"
      it "disallows non-alphanumeric letters" $ usernameValidationTest "df-.?=fg"

      it "cannot unset (username input prop is ignored if nothing)" $ do
        let
          user = User { _userId = "hello", _username = Just "username" }
          startAppState = addUser user initialAppState

          (result, endAppState) = runInState startAppState $
            UI.updateUserInfoLogic updateUser user (UI.UserInfoBody Nothing)

        endAppState ^? (userById "hello")._Just.username._Just `shouldBe` Just "username"
        result `shouldBe` (Right $ UI.UserInfoBody (Just "username"))

    describe "invite service" $ do
      let
        lookupUser :: I.LookupCriteria -> S.State AS.AppState (Maybe AS.User)
        lookupUser (I.ById uid) = use $ userById uid
        lookupUser (I.ByUsername uname) = use $ userByUsername uname

        lookupInvites :: AS.UserId -> S.State AS.AppState [AS.Invite]
        lookupInvites = S.gets . AS.findInvitesForUser

        addInvite :: AS.Invite -> S.State AS.AppState ()
        addInvite = S.modify . AS.addInvite

      it "disallows getting invites if no username set" $ do
        let
          user = fromJust $ getUserById "user1" testAppState
          (result, _) = runInState testAppState $ I.getInvitesLogic lookupInvites user

        result `shouldBe` Left (forbidden403, "Must set username first")

      it "fetches invites for user" $ do
        let
          invite1 = Invite { _player1 = "user2", _player2 = "user3" }
          invite2 = Invite { _player1 = "user3", _player2 = "user2" }
          invite3 = Invite { _player1 = "someone", _player2 = "else" }

          startAppState = AS.addInvite invite1 $ AS.addInvite invite2 $ AS.addInvite invite3 $ testAppState
          user = fromJust $ getUserById "user2" startAppState

          (Right invites, _) = runInState startAppState $ I.getInvitesLogic lookupInvites user

        invites `shouldContain` [invite1]
        invites `shouldContain` [invite2]
        invites `shouldNotContain` [invite3]

      it "disallows creating invites if no username set" $ do
        let
          startAppState = testAppState
          user = fromJust $ getUserById "user1" startAppState

          (result, endAppState) = runInState startAppState $
            I.createInviteLogic lookupUser addInvite user (I.InviteBodyUserId "user3")

        findInvitesForUser "user1" endAppState `shouldBe` []
        findInvitesForUser "user3" endAppState `shouldBe` []
        result `shouldBe` Left (forbidden403, "Must set username first")

      it "creates invites by user ID" $ do
        let
          startAppState = testAppState
          user = fromJust $ getUserById "user2" startAppState

          (result, endAppState) = runInState startAppState $
            I.createInviteLogic lookupUser addInvite user (I.InviteBodyUserId "user3")

          expectedInvite = Invite { _player1 = "user2", _player2 = "user3" }

        findInvitesForUser "user2" endAppState `shouldBe` [expectedInvite]
        findInvitesForUser "user3" endAppState `shouldBe` [expectedInvite]
        result `shouldBe` Right expectedInvite

      it "creates invites by username" $ do
        let
          startAppState = testAppState
          user = fromJust $ getUserById "user2" startAppState

          (result, endAppState) = runInState startAppState $
            I.createInviteLogic lookupUser addInvite user (I.InviteBodyUsername "anotheruser")

          expectedInvite = Invite { _player1 = "user2", _player2 = "user3" }

        findInvitesForUser "user2" endAppState `shouldBe` [expectedInvite]
        findInvitesForUser "user3" endAppState `shouldBe` [expectedInvite]
        result `shouldBe` Right expectedInvite

      it "reports error if no user with given ID exists" $ do
        let
          startAppState = testAppState
          user = fromJust $ getUserById "user2" startAppState

          (result, endAppState) = runInState startAppState $
            I.createInviteLogic lookupUser addInvite user (I.InviteBodyUserId "id-does-not-exist")

        findInvitesForUser "user2" endAppState `shouldBe` []
        findInvitesForUser "user3" endAppState `shouldBe` []
        result `shouldBe` Left (badRequest400, "No such user")

      it "reports error if no user with given username exists" $ do
        let
          startAppState = testAppState
          user = fromJust $ getUserById "user2" startAppState

          (result, endAppState) = runInState startAppState $
            I.createInviteLogic lookupUser addInvite user (I.InviteBodyUsername "uname-does-not-exist")

        findInvitesForUser "user2" endAppState `shouldBe` []
        findInvitesForUser "user3" endAppState `shouldBe` []
        result `shouldBe` Left (badRequest400, "No such user")

  appStateTVar <- runIO $ newTVarIO testAppState

  describe "REST app" $ beforeAll_ setupEnv $ before_ (resetAppState appStateTVar) $ with (createApp Test appStateTVar) $ do
    describe "/" $ do
      it "should work OK" $ do
        get "/" `shouldRespondWith` "hello"
