{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeOperators #-}

module Monad.App where

import Control.Monad.Except (throwError)
import Control.Monad.IO.Class (MonadIO(..))
import Control.Monad.Logger (LoggingT)
import Control.Monad.Reader (ReaderT(..), ask)
import Database.Persist.Sql (SqlPersistT)
import Database.Redis (runRedis, connect, Redis)
import Servant.Server ((:~>)(..), Handler, Handler(..))

import Cache (RedisInfo)
import Database (runPGAction, PGInfo)
import Errors (runWithServantHandler)
import Monad.Cache (MonadCache(..))
import Monad.Database (MonadDatabase(..))

newtype AppMonad a = AppMonad (ReaderT RedisInfo (SqlPersistT (LoggingT IO)) a)
  deriving (Functor, Applicative, Monad)

instance MonadIO AppMonad where
  liftIO action = AppMonad $ ReaderT (const (liftIO action))

instance MonadDatabase AppMonad where
  fetchUserDB = liftSqlPersistT . fetchUserDB
  createUserDB = liftSqlPersistT . createUserDB
  deleteUserDB = liftSqlPersistT . deleteUserDB
  fetchArticleDB = liftSqlPersistT . fetchArticleDB
  createArticleDB = liftSqlPersistT . createArticleDB
  deleteArticleDB = liftSqlPersistT . deleteArticleDB
  fetchArticlesByAuthor = liftSqlPersistT . fetchArticlesByAuthor
  fetchRecentArticles = liftSqlPersistT fetchRecentArticles

liftSqlPersistT :: SqlPersistT (LoggingT IO) a -> AppMonad a
liftSqlPersistT action = AppMonad $ ReaderT (const action)

instance MonadCache AppMonad where
  cacheUser uid user = liftRedis (cacheUser uid user)
  fetchCachedUser = liftRedis . fetchCachedUser 
  deleteCachedUser = liftRedis . deleteCachedUser

liftRedis :: Redis a -> AppMonad a
liftRedis action = do
  info <- AppMonad ask
  connection <- liftIO $ connect info
  liftIO $ runRedis connection action

runAppAction :: PGInfo -> RedisInfo -> AppMonad a -> IO a
runAppAction pgInfo redisInfo (AppMonad action) = runPGAction pgInfo $ runReaderT action redisInfo

transformAppToHandler :: PGInfo -> RedisInfo -> AppMonad :~> Handler
transformAppToHandler pgInfo redisInfo = NT $ \action -> do
  result <- liftIO (runWithServantHandler (runAppAction pgInfo redisInfo action))
  Handler $ either throwError return result
