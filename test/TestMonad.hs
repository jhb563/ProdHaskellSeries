{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeOperators #-}

module TestMonad where

import           Control.Exception.Safe (SomeException, Exception, handleAny)
import           Control.Monad.Except (throwError)
import           Control.Concurrent.MVar (MVar, readMVar, swapMVar)
import           Control.Lens (view, _1, _2, _3)
import           Control.Monad.State (StateT, get, put, runStateT, void)
import           Control.Monad.IO.Class (liftIO, MonadIO)
import           Data.Int (Int64)
import           Data.List (sortBy)
import qualified Data.Map as Map
import           Data.ByteString.Lazy.Char8 (pack)
import           Database.Persist.Postgresql (toSqlKey, fromSqlKey)
import           Servant.Server ((:~>)(..), Handler, ServantErr(..), Handler(..), err500,
                                 runHandler)

import           Cache (RedisInfo)
import           Database (PGInfo)
import           Monad.Cache (MonadCache(..))
import           Monad.Database(MonadDatabase(..))
import           Schema
import           Types (KeyVal(..))

type UserMap = Map.Map Int64 User
type ArticleMap = Map.Map Int64 Article

newtype TestMonad a = TestMonad (StateT (UserMap, ArticleMap, UserMap) IO a)
  deriving (Functor, Applicative, Monad)

instance MonadIO TestMonad where
  liftIO action = TestMonad $ liftIO action

transformTestToHandler :: MVar (UserMap, ArticleMap, UserMap) -> TestMonad :~> Handler
transformTestToHandler sharedMap = NT $ \(TestMonad action) -> do
  result <- liftIO $ handleAny handler $
    runStateTWithPointer action sharedMap 
  Handler $ either throwError return result
  where
    handler :: SomeException -> IO (Either ServantErr a)
    handler e = return $ Left $ err500 { errBody = pack (show e) }

runStateTWithPointer :: (Exception e, MonadIO m) => StateT s m a -> MVar s -> m (Either e a)
runStateTWithPointer action ref = do
  env <- liftIO $ readMVar ref
  (val, newEnv) <- runStateT action env
  void $ liftIO $ swapMVar ref newEnv
  return $ Right val

instance MonadDatabase TestMonad where
  fetchUserDB uid = TestMonad $ do
    userDB <- (view _1) <$> get
    return $ Map.lookup uid userDB
  createUserDB user = TestMonad $ do
    (userDB, articleDB, userCache) <- get
    let newUid = if Map.null userDB
          then 1
          else 1 + (fst . Map.findMax) userDB
    let userDB' = Map.insert newUid user userDB
    put (userDB', articleDB, userCache)
    return newUid
  deleteUserDB uid = TestMonad $ do
    (userDB, articleDB, userCache) <- get
    let userDB' = Map.delete uid userDB
    put (userDB', articleDB, userCache)
  fetchArticleDB aid = TestMonad $ do
    articleDB <- (view _2) <$> get
    return $ Map.lookup aid articleDB
  createArticleDB article = TestMonad $ do
    (userDB, articleDB, userCache) <- get
    let newAid = if Map.null articleDB
          then 1
          else 1 + (fst . Map.findMax) articleDB
    let articleDB' = Map.insert newAid article articleDB
    put (userDB, articleDB', userCache)
    return newAid
  deleteArticleDB aid = TestMonad $ do
    (userDB, articleDB, userCache) <- get
    let articleDB' = Map.delete aid articleDB
    put (userDB, articleDB', userCache)
  fetchArticlesByAuthor uid = TestMonad $ do
    articleDB <- (view _2) <$> get
    return $ map KeyVal (filter articleByAuthor (Map.toList articleDB))
    where
      articleByAuthor (_, article) = articleAuthorId article == toSqlKey uid
  fetchRecentArticles = TestMonad $ do
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

instance MonadCache TestMonad where
  cacheUser uid user = TestMonad $ do
    (userDB, articleDB, userCache) <- get
    let userCache' = Map.insert uid user userCache
    put (userDB, articleDB, userCache')
  fetchCachedUser uid = TestMonad $ do
    userCache <- (view _3) <$> get
    return $ Map.lookup uid userCache
  deleteCachedUser uid = TestMonad $ do
    (userDB, articleDB, userCache) <- get
    let userCache' = Map.delete uid userCache
    put (userDB, articleDB, userCache')
