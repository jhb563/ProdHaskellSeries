{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Database where

import           Control.Monad.Logger (runStdoutLoggingT, MonadLogger, LoggingT,
                                       LogLevel(..), filterLogger)
import           Control.Monad.Reader (runReaderT)
import           Control.Monad.IO.Class (MonadIO)
import           Data.Maybe (listToMaybe)
import           Database.Esqueleto (select, from, where_, (^.), val, (==.), on,
                                     InnerJoin(..), limit, orderBy, desc)
import           Database.Persist (get, insert, delete, entityVal, Entity(..))
import           Database.Persist.Sql (fromSqlKey, toSqlKey, ToBackendKey, SqlBackend)
import           Database.Persist.Postgresql (ConnectionString, withPostgresqlConn,
                                              runMigration, SqlPersistT)

import           Monad.Database (MonadDatabase(..))
import           Schema
import           Types (KeyVal(..))

type PGInfo = ConnectionString

localConnString :: PGInfo
localConnString = "host=127.0.0.1 port=5432 user=postgres dbname=postgres"

logFilter :: a -> LogLevel -> Bool
logFilter _ LevelError     = True
logFilter _ LevelWarn      = True
logFilter _ LevelInfo      = True
logFilter _ LevelDebug     = False
logFilter _ (LevelOther _) = False

runPGAction :: PGInfo -> SqlPersistT (LoggingT IO) a -> IO a
runPGAction connectionString action = runStdoutLoggingT $ filterLogger logFilter $
  withPostgresqlConn connectionString $ \backend ->
    runReaderT action backend

-- This is IO since in a real application we'd want to configure it.
fetchPostgresConnection :: IO PGInfo
fetchPostgresConnection = return localConnString

instance (MonadIO m, MonadLogger m) => MonadDatabase (SqlPersistT m) where
  fetchUserDB uid = get (toSqlKey uid)
  createUserDB user = fromSqlKey <$> insert user
  deleteUserDB uid = delete (toSqlKey uid :: Key User)
  fetchArticleDB aid = ((fmap entityVal) . listToMaybe) <$> (select . from $ \articles -> do
    where_ (articles ^. ArticleId ==. val (toSqlKey aid))
    return articles)
  createArticleDB article = fromSqlKey <$> insert article
  deleteArticleDB aid = delete (toSqlKey aid :: Key Article)
  fetchArticlesByAuthor uid = do
    entities <- select . from $ \articles -> do
      where_ (articles ^. ArticleAuthorId ==. val (toSqlKey uid))
      return articles
    return $ unEntity <$> entities
  fetchRecentArticles = do
    tuples <- select . from $ \(users `InnerJoin` articles) -> do
      on (users ^. UserId ==. articles ^. ArticleAuthorId)
      orderBy [desc (articles ^. ArticlePublishedTime)]
      limit 10
      return (users, articles)
    return $ (\(userEntity, articleEntity) -> (unEntity userEntity, unEntity articleEntity)) <$> tuples

unEntity :: (ToBackendKey SqlBackend a) => Entity a -> KeyVal a
unEntity (Entity id_ val_) = KeyVal (fromSqlKey id_, val_)

migrateDB :: PGInfo -> IO ()
migrateDB connString = runPGAction connString (runMigration migrateAll)
