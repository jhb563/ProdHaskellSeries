{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}

module Eff.Database where

import           Control.Monad.Freer (Member, Eff, send, runNat)
import           Control.Monad.Logger (LoggingT)
import           Data.Int (Int64)
import           Data.Maybe (listToMaybe)
import           Database.Esqueleto (select, from, where_, (^.), val, (==.), on,
                                     InnerJoin(..), limit, orderBy, desc)
import           Database.Persist (get, insert, delete, entityVal, Entity(..))
import           Database.Persist.Sql (fromSqlKey, toSqlKey, ToBackendKey, SqlBackend)
import           Database.Persist.Postgresql (SqlPersistT)

import           Schema
import           Types (KeyVal(..))

data Database a where
  FetchUserDB :: Int64 -> Database (Maybe User)
  CreateUserDB :: User -> Database Int64
  DeleteUserDB :: Int64 -> Database ()
  FetchArticleDB :: Int64 -> Database (Maybe Article)
  CreateArticleDB :: Article -> Database Int64
  DeleteArticleDB :: Int64 -> Database ()
  FetchArticlesByAuthor :: Int64 -> Database [KeyVal Article]
  FetchRecentArticles :: Database [(KeyVal User, KeyVal Article)]

fetchUserDB :: (Member Database r) => Int64 -> Eff r (Maybe User) 
fetchUserDB = send . FetchUserDB

createUserDB :: (Member Database r) => User -> Eff r Int64 
createUserDB = send . CreateUserDB

deleteUserDB :: (Member Database r) => Int64 -> Eff r ()
deleteUserDB = send . DeleteUserDB

fetchArticleDB :: (Member Database r) => Int64 -> Eff r (Maybe Article)
fetchArticleDB = send . FetchArticleDB

createArticleDB :: (Member Database r) => Article -> Eff r Int64
createArticleDB = send . CreateArticleDB

deleteArticleDB :: (Member Database r) => Int64 -> Eff r ()
deleteArticleDB = send . DeleteArticleDB

fetchArticlesByAuthor :: (Member Database r) => Int64 -> Eff r [KeyVal Article]
fetchArticlesByAuthor = send . FetchArticlesByAuthor

fetchRecentArticles :: (Member Database r) => Eff r [(KeyVal User, KeyVal Article)]
fetchRecentArticles = send FetchRecentArticles

runDatabase :: (Member (SqlPersistT (LoggingT IO)) r) => Eff (Database ': r) a -> Eff r a
runDatabase = runNat databaseToSql
  where
    databaseToSql :: Database a -> SqlPersistT (LoggingT IO) a
    databaseToSql (FetchUserDB uid) = get (toSqlKey uid)
    databaseToSql (CreateUserDB user) = fromSqlKey <$> insert user
    databaseToSql (DeleteUserDB uid) = delete (toSqlKey uid :: Key User)
    databaseToSql (FetchArticleDB aid) = ((fmap entityVal) . listToMaybe) <$> (select . from $ \articles -> do
      where_ (articles ^. ArticleId ==. val (toSqlKey aid))
      return articles)
    databaseToSql (CreateArticleDB article) = fromSqlKey <$> insert article
    databaseToSql (DeleteArticleDB aid) = delete (toSqlKey aid :: Key Article)
    databaseToSql (FetchArticlesByAuthor uid) = do
      entities <- select . from $ \articles -> do
        where_ (articles ^. ArticleAuthorId ==. val (toSqlKey uid))
        return articles
      return $ unEntity <$> entities
    databaseToSql FetchRecentArticles = do
      tuples <- select . from $ \(users `InnerJoin` articles) -> do
        on (users ^. UserId ==. articles ^. ArticleAuthorId)
        orderBy [desc (articles ^. ArticlePublishedTime)]
        limit 10
        return (users, articles)
      return $ (\(userEntity, articleEntity) -> (unEntity userEntity, unEntity articleEntity)) <$> tuples

unEntity :: (ToBackendKey SqlBackend a) => Entity a -> KeyVal a
unEntity (Entity id_ val_) = KeyVal (fromSqlKey id_, val_)
