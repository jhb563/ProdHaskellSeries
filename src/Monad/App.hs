{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Monad.App where

import Control.Monad.IO.Class (MonadIO(..))
import Control.Monad.Logger (LoggingT)
import Control.Monad.Reader (ReaderT(..), ask)
import Database.Persist.Sql (SqlPersistT)
import Database.Redis (runRedis, connect, Redis)

import Cache ()
import Database ()
import Monad.Cache (MonadCache(..))
import Monad.Database (MonadDatabase(..))
import Types (RedisInfo)

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
