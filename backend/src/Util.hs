{-# LANGUAGE OverloadedStrings #-}

module Util where

import Data.Aeson
import Data.Bifunctor
import Control.Monad (unless, forM_)
import qualified Data.Map.Strict as M
import Control.Monad.Trans (liftIO, MonadIO)
import Control.Concurrent.STM hiding (check)
import Web.Scotty as S
import qualified Data.Text as T

type Updater m s r = (s -> (r, s)) -> m r

toMultiMap :: Ord f => [(f, e)] -> M.Map f [e]
toMultiMap = M.fromListWith (<>) . wrapSecond . reverse
  where
    wrapSecond :: [(f, e)] -> [(f, [e])]
    wrapSecond = map . second $ pure

atomically' :: MonadIO m => STM a -> m a
atomically' = liftIO . atomically

stateTVar :: TVar s -> Updater STM s a
stateTVar var f = do
  s <- readTVar var
  let (a, s') = f s
  writeTVar var s'
  return a
{-# INLINE stateTVar #-}

queryTVar :: MonadIO m => TVar s -> (s -> a) -> m a
queryTVar tvar f = liftIO $ f <$> readTVarIO tvar

modifyTVarState :: MonadIO m => TVar s -> (s -> (a, s)) -> m a
modifyTVarState tvar f = atomically' $ stateTVar tvar f

checkError cond status errMsg = unless cond $ sendErrorAndFinish status errMsg
guardError status errMsg cond = checkError cond status errMsg

sendErrorAndFinish status errMsg = do
  S.status status
  S.json $ object ["error" .= errMsg]
  S.finish

adjustMatching :: (a -> Bool) -> (Maybe a -> Maybe a) -> [a] -> [a]
adjustMatching _ f [] = maybe [] pure $ f Nothing
adjustMatching pred f (a:as)
  | pred a = maybe as (:as) $ f (Just a)
  | otherwise = a:adjustMatching pred f as

pusherizedUserId :: T.Text -> T.Text
pusherizedUserId = T.map replaceChar
  where
    replaceChar '|' = ';'
    replaceChar c = c
