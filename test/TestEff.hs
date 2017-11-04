{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE GADTs            #-}
{-# LANGUAGE TypeOperators    #-}
{-# LANGUAGE FlexibleContexts #-}

module TestEff where

import           Control.Concurrent.MVar (MVar)
import           Control.Exception.Safe (SomeException, Exception, handleAny)
import           Control.Lens (view, _1, _2, _3)
import           Control.Monad.Except (throwError)
import           Control.Monad.Freer (Eff, Member, runNat, runM)
import           Control.Monad.IO.Class (liftIO, MonadIO)
import           Control.Monad.State (StateT, get, put)
import           Data.ByteString.Lazy.Char8 (pack)
import           Data.List (sortBy)
import qualified Data.Map as Map
import           Database.Persist.Postgresql (toSqlKey, fromSqlKey)
import           Servant.Server ((:~>)(..), Handler, ServantErr(..), Handler(..), err500,
                                 runHandler)

{-import           Control.Exception.Safe (SomeException, Exception, handleAny)
import           Control.Monad.Except (throwError)
import           Control.Monad.State (StateT, get, put, runStateT, void)
import           Control.Monad.IO.Class (liftIO, MonadIO)
import           Data.List (sortBy)
-}

import           Eff.Cache (Cache(..))
import           Eff.Database(Database(..))
import           Schema
import           TestMonad (UserMap, ArticleMap, runStateTWithPointer)
import           Types (KeyVal(..))


transformTestEffToHandler :: MVar (UserMap, ArticleMap, UserMap) -> Eff '[Cache, Database, StateT (UserMap, ArticleMap, UserMap) IO] :~> Handler
transformTestEffToHandler sharedMap = NT $ \action -> do
  let stateAct = (runTestDatabase . runTestCache) action
  result <- liftIO (handleAny handler (runRight stateAct))
  Handler $ either throwError return result
  where
    handler :: SomeException -> IO (Either ServantErr a)
    handler e = return $ Left $ err500 { errBody = pack (show e)}

    runRight :: (Exception e) => Eff '[StateT (UserMap, ArticleMap, UserMap) IO] a -> IO (Either e a)
    runRight action = do
      let stateAction = runM action
      runStateTWithPointer stateAction sharedMap
    

runTestDatabase :: (Member (StateT (UserMap, ArticleMap, UserMap) IO) r) => Eff (Database ': r) a -> Eff r a
runTestDatabase = runNat databaseToState
  where
    databaseToState :: Database a -> StateT (UserMap, ArticleMap, UserMap) IO a
    databaseToState (FetchUserDB uid) = do
      userDB <- (view _1) <$> get
      return $ Map.lookup uid userDB
    databaseToState (CreateUserDB user) = do
      (userDB, articleDB, userCache) <- get
      let newUid = if Map.null userDB
            then 1
            else 1 + (fst . Map.findMax) userDB
      let userDB' = Map.insert newUid user userDB
      put (userDB', articleDB, userCache)
      return newUid
    databaseToState (DeleteUserDB uid) = do
      (userDB, articleDB, userCache) <- get
      let userDB' = Map.delete uid userDB
      put (userDB', articleDB, userCache)
    databaseToState (FetchArticleDB aid) = do
      articleDB <- (view _2) <$> get
      return $ Map.lookup aid articleDB
    databaseToState (CreateArticleDB article) = do
      (userDB, articleDB, userCache) <- get
      let newAid = if Map.null articleDB
            then 1
            else 1 + (fst . Map.findMax) articleDB
      let articleDB' = Map.insert newAid article articleDB
      put (userDB, articleDB', userCache)
      return newAid
    databaseToState (DeleteArticleDB aid) = do
      (userDB, articleDB, userCache) <- get
      let articleDB' = Map.delete aid articleDB
      put (userDB, articleDB', userCache)
    databaseToState (FetchArticlesByAuthor uid) = do
      articleDB <- (view _2) <$> get
      return $ map KeyVal (filter articleByAuthor (Map.toList articleDB))
      where
        articleByAuthor (_, article) = articleAuthorId article == toSqlKey uid
    databaseToState FetchRecentArticles = do
      (userDB, articleDB, _) <- get
      let recentArticles = take 10 (sortBy orderByTimestamp (Map.toList articleDB)) 
      return $ map (matchWithAuthor userDB) recentArticles
      where
        orderByTimestamp (_, article1) (_, article2) =
          articlePublishedTime article2 `compare` articlePublishedTime article1
        matchWithAuthor userDB (aid, article) =
          case Map.lookup (fromSqlKey (articleAuthorId article)) userDB of
            Nothing -> error "Found article with no user" 
            Just u -> (KeyVal (fromSqlKey (articleAuthorId article), u), KeyVal (aid, article))

runTestCache :: (Member (StateT (UserMap, ArticleMap, UserMap) IO) r) => Eff (Cache ': r) a -> Eff r a
runTestCache = runNat cacheToState
  where
    cacheToState :: Cache a -> StateT (UserMap, ArticleMap, UserMap) IO a
    cacheToState (CacheUser uid user) = do
      (userDB, articleDB, userCache) <- get
      let userCache' = Map.insert uid user userCache
      put (userDB, articleDB, userCache')
    cacheToState (FetchCachedUser uid) = do
      userCache <- (view _3) <$> get
      return $ Map.lookup uid userCache
    cacheToState (DeleteCachedUser uid) = do
      (userDB, articleDB, userCache) <- get
      let userCache' = Map.delete uid userCache
      put (userDB, articleDB, userCache')
