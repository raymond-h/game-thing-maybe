{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE FlexibleInstances #-}

import Data.Maybe (isJust, fromJust)
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
import Test.QuickCheck as Q
import Test.QuickCheck.Poly
import Data.Monoid
import Data.Foldable
import Data.Functor (($>))
import System.Environment
import Control.Concurrent.STM
import qualified Data.Text as T
import qualified Data.Map.Strict as M
import qualified Network.Pusher as P

import qualified Database.Persist as Ps
import qualified Database.Persist.Sql as Ps
import Database.Persist.Sqlite (createSqlitePool)
import Control.Monad.Logger (runNoLoggingT)
import Data.Pool (Pool)
import qualified Database as DB

import Lib (createApp, Environment(..))
import AppState as AS
import qualified Game as G
import qualified PusherCommon as PC
import qualified Service.UserInfo as UI
import qualified Service.Invite as I
import qualified Service.PusherAuth as PA
import qualified Service.GameState as GS
import Util (adjustMatching, atFieldLabelModifier)

instance Arbitrary AS.User where
  arbitrary = AS.User <$> arbitrary <*> arbitrary

instance Arbitrary (Ps.Entity DB.User) where
  arbitrary = Ps.Entity <$> (DB.UserKey <$> arbitrary) <*> arbitrary

instance Arbitrary DB.User where
  arbitrary = DB.User <$> arbitrary

instance Arbitrary AS.Invite where
  arbitrary = AS.Invite <$> arbitrary <*> arbitrary <*> arbitrary

instance Arbitrary (Ps.Entity DB.Invite) where
  arbitrary = Ps.Entity <$> (Ps.toSqlKey <$> arbitrary) <*> arbitrary

instance Arbitrary DB.Invite where
  arbitrary = DB.Invite <$> (DB.UserKey <$> arbitrary) <*> (DB.UserKey <$> arbitrary)

instance Arbitrary AS.GameAppState where
  arbitrary = AS.GameAppState <$> arbitrary <*> arbitrary <*> arbitrary

instance Arbitrary G.State where
  arbitrary = G.State <$> arbitrary <*> arbitrary <*> arbitrary

instance Arbitrary G.PlayerState where
  arbitrary = G.PlayerState <$> arbitrary <*> arbitrary <*> arbitrary

instance Arbitrary G.Piece where
  arbitrary = G.Piece <$> arbitrary

instance Arbitrary G.Player where
  arbitrary = Q.elements [G.Player1, G.Player2]

instance Arbitrary (Ps.Entity DB.GameAppState) where
  arbitrary = Ps.Entity <$> (Ps.toSqlKey <$> arbitrary) <*> arbitrary

instance Arbitrary DB.GameAppState where
  arbitrary = DB.GameAppState <$> (DB.UserKey <$> arbitrary) <*> (DB.UserKey <$> arbitrary) <*> arbitrary

testAppState :: AppState
testAppState = initialAppState {
  _users = M.fromList [
    ("user1", User { _userId = "user1", _userUsername = Nothing }),
    ("user2", User { _userId = "user2", _userUsername = Just "testuser2" }),
    ("user3", User { _userId = "user3", _userUsername = Just "anotheruser" })
  ]
}

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

main :: IO ()
main = hspec $ do
  describe "Database <-> AppState" $ do
    prop "User isomorphism" $ \user ->
      (DB.fromDbUser $ DB.toDbUser $ user) `shouldBe` user

    prop "User isomorphism 2" $ \user ->
      (DB.toDbUser $ DB.fromDbUser $ user) `shouldBe` user

    prop "Invite entity isomorphism (assuming ID is present)" $ \invite -> isJust (AS._inviteId invite) ==>
      (DB.fromDbInviteEntity $ DB.toDbInviteEntity $ invite) `shouldBe` invite

    prop "Invite entity isomorphism 2" $ \invite ->
      (DB.toDbInviteEntity $ DB.fromDbInviteEntity $ invite) `shouldBe` invite

    prop "GameAppState isomorphism" $ \gas ->
      (DB.fromDbGameAppState $ DB.toDbGameAppState $ gas) `shouldBe` gas

    prop "GameAppState isomorphism 2" $ \gas ->
      (DB.toDbGameAppState $ DB.fromDbGameAppState $ gas) `shouldBe` gas

    prop "GameAppState ID isomorphism" $ \gameId ->
      (DB.fromDbGameAppStateId $ DB.toDbGameAppStateId $ gameId) `shouldBe` gameId

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
          newUser = User { _userId = "hello", _userUsername = Just "Hello-Person" }
          updatedAppState = appState & userById "hello" ?~ newUser

        _users updatedAppState `shouldBe` M.singleton "hello" newUser

      it "allows setting to Nothing" $ do
        let
          (_, appState) = ensureUser "hello" initialAppState
          updatedAppState = appState & userById "hello" .~ Nothing

        updatedAppState^.users `shouldBe` M.empty

      it "allows modifying existing user" $ do
        let
          (_, appState) = ensureUser "hello" initialAppState
          updatedAppState = appState & userById "hello" . traverse . userUsername ?~ "Banana"

        appState ^.. users . at "hello" . _Just . userUsername `shouldBe` [Nothing]
        updatedAppState ^.. users . at "hello" . _Just . userUsername `shouldBe` [Just "Banana"]

    describe "userByUsername lens" $ do
      it "allows getting" $ do
        let
          user = User { _userId = "hello", _userUsername = Just "SomeUname" }
          appState = initialAppState & AS.addUser user

        appState ^. userByUsername "SomeUname" `shouldBe` Just user

      it "allows setting to Just user" $ do
        let
          appState = initialAppState
          newUser = User { _userId = "hello", _userUsername = Just "Hello-Person" }
          updatedAppState = appState & userByUsername "Hello-Person" ?~ newUser

        _users updatedAppState `shouldBe` M.singleton "hello" newUser

      it "allows setting to Nothing" $ do
        let
          user = User { _userId = "hello", _userUsername = Just "SomeUname" }
          appState = initialAppState & AS.addUser user
          updatedAppState = appState & userByUsername "SomeUname" .~ Nothing

        updatedAppState^.users `shouldBe` M.empty

      it "allows modifying existing user" $ do
        let
          user = User { _userId = "hello", _userUsername = Just "SomeUname" }
          appState = initialAppState & AS.addUser user
          updatedAppState = appState & userByUsername "SomeUname" . traverse . userUsername ?~ "Banana"

        updatedAppState^.users.(to M.elems) `shouldBe` [User { _userId = "hello", _userUsername = Just "Banana" }]

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

    describe "atFieldLabelModifier" $ do
      it "works" $ do
        atFieldLabelModifier "User" "_userSomeField" `shouldBe` "someField"
        atFieldLabelModifier "MultiWordName" "_multiWordNameSomeField" `shouldBe` "someField"

      it "ignores fields without record name as prefix" $ do
        atFieldLabelModifier "User" "someField" `shouldBe` "someField"
        atFieldLabelModifier "MultiWordName" "_someField" `shouldBe` "_someField"

  describe "App logic" $ do
    let
      pushClient :: [PC.EventChannel] -> P.Event -> P.EventData -> S.State AS.AppState ()
      pushClient chans ev evData = AS.pusherEventsSent <>= [(PC.toChannel <$> chans, ev, evData)]

    describe "user info service" $ do
      let
        isUsernameInUse :: T.Text -> S.State AS.AppState Bool
        isUsernameInUse uname = uses (AS.userByUsername uname) isJust

        updateUser :: User -> S.State AS.AppState ()
        updateUser = S.modify . AS.updateUser

        testUpdateUserInfo = UI.updateUserInfoLogic isUsernameInUse updateUser pushClient

      it "can get user info" $ do
        let
          user = User { _userId = "whatever", _userUsername = Just "cool-username" }

        UI.getUserInfoLogic user `shouldBe` UI.UserInfoBody (Just "cool-username")

      it "can set username" $ do
        let
          user = User { _userId = "hello", _userUsername = Nothing }
          startAppState = addUser user initialAppState

          (result, endAppState) = runInState startAppState $
            testUpdateUserInfo user (UI.UserInfoBody $ Just "username")

        endAppState ^? (userById "hello")._Just.userUsername._Just `shouldBe` Just "username"
        endAppState^.pusherEventsSent `shouldBe` [([P.Channel P.Private "hello-user-info"], "update-user-info", "{\"username\":\"username\"}")]
        result `shouldBe` (Right $ UI.UserInfoBody (Just "username"))

      let
        usernameValidationTest un = do
          let
            user = User { _userId = "hello", _userUsername = Nothing }
            startAppState = addUser user initialAppState

            (result, endAppState) = runInState startAppState $
              testUpdateUserInfo user (UI.UserInfoBody $ Just un)

          endAppState ^? (userById "hello")._Just.userUsername._Just `shouldBe` Nothing
          endAppState^.pusherEventsSent `shouldBe` []
          result ^? _Left.(at "username") `shouldSatisfy` isJust

      it "disallows too long username" $ usernameValidationTest "veryveryverylongusername"
      it "disallows too short username" $ usernameValidationTest "u"
      it "disallows non-alphanumeric letters" $ usernameValidationTest "df-.?=fg"

      it "cannot unset (username input prop is ignored if nothing)" $ do
        let
          user = User { _userId = "hello", _userUsername = Just "username" }
          startAppState = addUser user initialAppState

          (result, endAppState) = runInState startAppState $
            testUpdateUserInfo user (UI.UserInfoBody Nothing)

        endAppState ^? (userById "hello")._Just.userUsername._Just `shouldBe` Just "username"
        endAppState^.pusherEventsSent `shouldBe` []
        result `shouldBe` (Right $ UI.UserInfoBody (Just "username"))

      it "disallows setting username to existing username" $ do
        let
          user1 = User { _userId = "hello", _userUsername = Nothing }
          user2 = User { _userId = "other", _userUsername = Just "Existing" }
          startAppState = addUser user2 $ addUser user1 $ initialAppState

          (result, endAppState) = runInState startAppState $
            testUpdateUserInfo user1 (UI.UserInfoBody $ Just "Existing")

        endAppState `shouldBe` startAppState
        endAppState^.pusherEventsSent `shouldBe` []
        result `shouldBe` Left (M.singleton "username" ["Username already in use"])

      it "does not disallow setting username to your current username" $ do
        let
          user1 = User { _userId = "hello", _userUsername = Just "Something" }
          user2 = User { _userId = "other", _userUsername = Just "Existing" }
          startAppState = addUser user2 $ addUser user1 $ initialAppState

          (result, endAppState) = runInState startAppState $
            testUpdateUserInfo user1 (UI.UserInfoBody $ Just "Something")

        endAppState `shouldBe` startAppState
        endAppState^.pusherEventsSent `shouldBe` []
        result `shouldBe` (Right $ UI.UserInfoBody (Just "Something"))

      prop "allows getting user info of other users" $ \mUname -> do
        let
          user1 = User { _userId = "hello", _userUsername = mUname }

          result = UI.getSpecificUserInfoLogic (Just user1)

        result `shouldBe` (Right $ UI.UserInfoBody mUname)

      it "gives appropriate error when user does not exist" $ do
        UI.getSpecificUserInfoLogic Nothing `shouldBe` Left (status404, "No such user")

    describe "invite service" $ do
      let
        lookupUser :: I.LookupCriteria -> S.State AS.AppState (Maybe AS.User)
        lookupUser (I.ById uid) = use $ userById uid
        lookupUser (I.ByUsername uname) = use $ userByUsername uname

        lookupInvites :: AS.UserId -> S.State AS.AppState [AS.Invite]
        lookupInvites = S.gets . AS.findInvitesForUser

        lookupInvite :: AS.Id -> S.State AS.AppState (Maybe AS.Invite)
        lookupInvite invId = S.gets $ find (\inv -> _inviteId inv == Just invId) . view invites

        addInvite :: AS.Invite -> S.State AS.AppState AS.Id
        addInvite = S.state . AS.addInvite

        removeInvite :: AS.Id -> S.State AS.AppState ()
        removeInvite invId = S.modify $ AS.deleteInvite invId

        addGame :: AS.UserId -> AS.UserId -> S.State AS.AppState AS.GameAppState
        addGame uid otherUid = S.state $ AS.createGame uid otherUid

        testGetInvites = I.getInvitesLogic lookupInvites
        testCreateInvite = I.createInviteLogic lookupUser addInvite pushClient
        testAcceptInvite = I.acceptInviteLogic lookupInvite removeInvite addGame pushClient

      it "disallows getting invites if no username set" $ do
        let
          (Just user) = getUserById "user1" testAppState
          (result, _) = runInState testAppState $ testGetInvites user

        result `shouldBe` Left (forbidden403, "Must set username first")

      it "fetches invites for user" $ do
        let
          invite1 = Invite { _inviteId = Just 1, _invitePlayer1 = "user2", _invitePlayer2 = "user3" }
          invite2 = Invite { _inviteId = Just 2, _invitePlayer1 = "user3", _invitePlayer2 = "user2" }
          invite3 = Invite { _inviteId = Just 3, _invitePlayer1 = "someone", _invitePlayer2 = "else" }

          startAppState = testAppState & invites .~ M.fromList [(1, invite1), (2, invite2), (3, invite3)]
          (Just user) = getUserById "user2" startAppState

          (Right userInvites, _) = runInState startAppState $ testGetInvites user

        userInvites `shouldContain` [invite1]
        userInvites `shouldContain` [invite2]
        userInvites `shouldNotContain` [invite3]

      it "disallows creating invites if no username set" $ do
        let
          startAppState = testAppState
          (Just user) = getUserById "user1" startAppState

          (result, endAppState) = runInState startAppState $
            testCreateInvite user (I.InviteBodyUserId "user3")

        findInvitesForUser "user1" endAppState `shouldBe` []
        findInvitesForUser "user3" endAppState `shouldBe` []
        endAppState^.pusherEventsSent `shouldBe` []
        result `shouldBe` Left (forbidden403, "Must set username first")

      it "creates invites by user ID" $ do
        let
          startAppState = testAppState
          (Just user) = getUserById "user2" startAppState

          (result, endAppState) = runInState startAppState $
            testCreateInvite user (I.InviteBodyUserId "user3")

          expectedInvite = Invite { _inviteId = Just 1, _invitePlayer1 = "user2", _invitePlayer2 = "user3" }

        findInvitesForUser "user2" endAppState `shouldBe` [expectedInvite]
        findInvitesForUser "user3" endAppState `shouldBe` [expectedInvite]
        endAppState^.pusherEventsSent `shouldMatchList` [
            ([P.Channel P.Private "user2-invites", P.Channel P.Private "user3-invites"], "update-invites", "")
          ]
        result `shouldBe` Right expectedInvite

      it "creates invites by username" $ do
        let
          startAppState = testAppState
          (Just user) = getUserById "user2" startAppState

          (result, endAppState) = runInState startAppState $
            testCreateInvite user (I.InviteBodyUsername "anotheruser")

          expectedInvite = Invite { _inviteId = Just 1, _invitePlayer1 = "user2", _invitePlayer2 = "user3" }

        findInvitesForUser "user2" endAppState `shouldBe` [expectedInvite]
        findInvitesForUser "user3" endAppState `shouldBe` [expectedInvite]
        endAppState^.pusherEventsSent `shouldMatchList` [
            ([P.Channel P.Private "user2-invites", P.Channel P.Private "user3-invites"], "update-invites", "")
          ]
        result `shouldBe` Right expectedInvite

      it "does not create invites to same user" $ do
        let
          startAppState = testAppState
          (Just user) = getUserById "user2" startAppState

          (result, endAppState) = runInState startAppState $
            testCreateInvite user (I.InviteBodyUserId "user2")

        findInvitesForUser "user2" endAppState `shouldBe` []
        endAppState^.pusherEventsSent `shouldBe` []
        result `shouldBe` Left (badRequest400, "Cannot invite yourself")

      it "reports error if no user with given ID exists" $ do
        let
          startAppState = testAppState
          (Just user) = getUserById "user2" startAppState

          (result, endAppState) = runInState startAppState $
            testCreateInvite user (I.InviteBodyUserId "id-does-not-exist")

        findInvitesForUser "user2" endAppState `shouldBe` []
        findInvitesForUser "user3" endAppState `shouldBe` []
        endAppState^.pusherEventsSent `shouldBe` []
        result `shouldBe` Left (badRequest400, "No such user")

      it "reports error if no user with given username exists" $ do
        let
          startAppState = testAppState
          (Just user) = getUserById "user2" startAppState

          (result, endAppState) = runInState startAppState $
            testCreateInvite user (I.InviteBodyUsername "uname-does-not-exist")

        findInvitesForUser "user2" endAppState `shouldBe` []
        findInvitesForUser "user3" endAppState `shouldBe` []
        endAppState^.pusherEventsSent `shouldBe` []
        result `shouldBe` Left (badRequest400, "No such user")

      it "allows accepting invite" $ do
        let
          invite = Invite { _inviteId = Just 5, _invitePlayer1 = "user2", _invitePlayer2 = "user3" }
          startAppState = testAppState & invites .~ M.singleton 5 invite
          (Just user) = getUserById "user3" startAppState

          (result, endAppState) = runInState startAppState $ testAcceptInvite user (I.AcceptInviteBody 5)

          expectedGame = GameAppState 1 ("user2", "user3") G.initialState

        endAppState^.invites `shouldBe` M.empty
        endAppState^.gameAppStates `shouldBe` M.singleton 1 expectedGame
        endAppState^.pusherEventsSent `shouldMatchList` [
            ([P.Channel P.Private "user2-invites", P.Channel P.Private "user3-invites"], "update-invites", "")
          ]
        result `shouldBe` Right expectedGame

      it "reports error if accepting non-existing invite" $ do
        let
          startAppState = testAppState
          (Just user) = getUserById "user2" startAppState

          (result, endAppState) = runInState startAppState $ testAcceptInvite user (I.AcceptInviteBody 5)

        endAppState^.gameAppStates `shouldBe` M.empty
        result `shouldBe` Left (status404, "No such invite")

      forM_ ["user1", "user2"] $ \uname ->
        it ("reports error if not exactly recipient of invite (" <> T.unpack uname <> ")") $ do
          let
            invite = Invite { _inviteId = Just 5, _invitePlayer1 = "user2", _invitePlayer2 = "user3" }
            startAppState = testAppState & invites .~ M.singleton 5 invite
            (Just user) = getUserById uname startAppState

            (result, endAppState) = runInState startAppState $ testAcceptInvite user (I.AcceptInviteBody 5)

          endAppState^.invites `shouldBe` M.singleton 5 invite
          endAppState^.gameAppStates `shouldBe` M.empty
          endAppState^.pusherEventsSent `shouldBe` []
          result `shouldBe` Left (status403, "User not recipient of invite")

    describe "pusher auth service" $ do
      it "allows authenticating a user for private channels" $ do
        let
          creds = P.Credentials {
              P.credentialsAppID = 123,
              P.credentialsAppKey = "some-key",
              P.credentialsAppSecret = "some-secret",
              P.credentialsCluster = Nothing
            }
          user = User { _userId = "hello", _userUsername = Just "SomeUname" }
          chan = P.Channel P.Private "hello-user-info"

        (PA.paResAuthToken . fromJust) (PA.pusherAuthenticateLogic creds user "123.456" chan)
          `shouldBe`
          (P.authenticatePrivate creds "123.456" chan)

      it "prevents authenticating for wrong channels" $ do
        let
          creds = undefined -- should never be used by the test
          user = User { _userId = "hello", _userUsername = Just "SomeUname" }

        PA.pusherAuthenticateLogic creds user "123.456" (P.Channel P.Private "user1-user-info") `shouldBe` Nothing

      it "prevents authenticating for public channels" $ do
        let
          creds = undefined -- should never be used by the test
          user = User { _userId = "hello", _userUsername = Just "SomeUname" }

        PA.pusherAuthenticateLogic creds user "123.456" (P.Channel P.Public "some-whatever-channel") `shouldBe` Nothing

    describe "game state service" $ do
      let
        lookupGame :: DB.GameAppStateId -> S.State AS.AppState (Maybe (Ps.Entity DB.GameAppState))
        lookupGame gameId = uses AS.gameAppStates $
          fmap DB.toDbGameAppState . find (\gas -> gas ^. AS.gameAppStateId == DB.fromDbGameAppStateId gameId)

        updateGame :: Ps.Entity DB.GameAppState -> S.State AS.AppState ()
        updateGame gameEntity = S.modify $ AS.gameAppStates . at (asGas^.gameAppStateId) ?~ asGas
          where
            asGas = DB.fromDbGameAppState gameEntity

        testPerformMove = GS.performMoveLogic lookupGame updateGame pushClient

      prop "allows getting game state" $ \gas -> do
        let
          result = GS.getGameStateLogic (Just $ DB.toDbGameAppState gas)

        result `shouldBe` Right (DB.toDbGameAppState gas)

      it "returns error if game does not exist" $ do
        let
          result = GS.getGameStateLogic Nothing

        result `shouldBe` Left (status404, "No such game")

      it "lets current player perform a move" $ do
        let
          gameId = 5
          game = AS.GameAppState {
              AS._gameAppStateId = gameId,
              AS._gameAppStatePlayers = ("user2", "user3"),
              AS._gameAppStateState = G.initialState
            }
          move = G.RollDice

          (Just expectedGameState) = G.performMove move (AS._gameAppStateState game)

          startAppState = testAppState & gameAppStates .~ M.singleton gameId game
          (Just user) = getUserById "user2" startAppState

          (result, endAppState) = runInState startAppState $
            testPerformMove (DB.toDbUser user) (DB.toDbGameAppStateId gameId) move

        endAppState ^. AS.gameAppStates . (at 5) `shouldNotBe` startAppState ^. AS.gameAppStates . (at 5)
        endAppState `shouldNotBe` startAppState
        endAppState^.pusherEventsSent `shouldMatchList` [
            ([P.Channel P.Public "game-5"], "update-state", "")
          ]
        result `shouldBe` (Right $ DB.toDbGameAppState $ game { AS._gameAppStateState = expectedGameState })

      it "returns error if game does not exist" $ do
        let
          startAppState = testAppState
          (Just user) = getUserById "user2" startAppState

          (result, endAppState) = runInState startAppState $
            testPerformMove (DB.toDbUser user) (DB.toDbGameAppStateId 5) G.RollDice

        endAppState `shouldBe` startAppState
        endAppState^.pusherEventsSent `shouldBe` []
        result `shouldBe` Left (status404, "No such game")

      it "does not let the player that is not the current player perform a move" $ do
        let
          gameId = 5
          game = AS.GameAppState {
              AS._gameAppStateId = gameId,
              AS._gameAppStatePlayers = ("user2", "user3"),
              AS._gameAppStateState = G.initialState
            }
          move = G.RollDice

          startAppState = testAppState & gameAppStates .~ M.singleton gameId game
          (Just user) = getUserById "user3" startAppState

          (result, endAppState) = runInState startAppState $
            testPerformMove (DB.toDbUser user) (DB.toDbGameAppStateId gameId) move

        endAppState `shouldBe` startAppState
        endAppState^.pusherEventsSent `shouldBe` []
        result `shouldBe` Left (status400, "Not this player's turn")

      it "does not let any user who is not a player perform a move" $ do
        let
          gameId = 5
          game = AS.GameAppState {
              AS._gameAppStateId = gameId,
              AS._gameAppStatePlayers = ("user2", "user3"),
              AS._gameAppStateState = G.initialState
            }
          move = G.RollDice

          startAppState = testAppState & gameAppStates .~ M.singleton gameId game
          (Just user) = getUserById "user1" startAppState

          (result, endAppState) = runInState startAppState $
            testPerformMove (DB.toDbUser user) (DB.toDbGameAppStateId gameId) move

        endAppState `shouldBe` startAppState
        endAppState^.pusherEventsSent `shouldBe` []
        result `shouldBe` Left (status400, "User not a player in this game")

      it "reports when invalid move performed" $ do
        let
          gameId = 5
          game = AS.GameAppState {
              AS._gameAppStateId = gameId,
              AS._gameAppStatePlayers = ("user2", "user3"),
              AS._gameAppStateState = G.initialState
            }
          -- adding a piece before rolling dice is no bueno, better report that to user
          move = G.AddPiece

          startAppState = testAppState & gameAppStates .~ M.singleton gameId game
          (Just user) = getUserById "user2" startAppState

          (result, endAppState) = runInState startAppState $
            testPerformMove (DB.toDbUser user) (DB.toDbGameAppStateId gameId) move

        endAppState `shouldBe` startAppState
        endAppState^.pusherEventsSent `shouldBe` []
        result `shouldBe` Left (status400, "Invalid move")

  dbPool <- runIO $ runNoLoggingT $ createSqlitePool ":memory:" 1

  describe "REST app" $ beforeAll_ setupEnv $ with (createApp Test dbPool) $ do
    describe "/" $ do
      it "should work OK" $ do
        get "/" `shouldRespondWith` "hello"
