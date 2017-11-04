{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}

module API where

import           Control.Monad.Freer (Eff, Member)
import           Control.Monad.Logger (LoggingT)
import           Data.Int (Int64)
import           Data.Proxy (Proxy(..))
import           Database.Persist.Sql (SqlPersistT)
import           Database.Redis (Redis)
import           Network.Wai.Handler.Warp (run)
import           Servant.API
import           Servant.Client
import           Servant.Server

import           Cache (fetchRedisConnection)
import           Database (fetchPostgresConnection)
import           Eff.App (transformEffToHandler)
import           Eff.Cache
import           Eff.Database
import           Schema
import           Types (KeyVal(..))

type FullAPI =
       "users" :> Capture "userid" Int64 :> Get '[JSON] User
  :<|> "users" :> ReqBody '[JSON] User :> Post '[JSON] Int64
  :<|> "articles" :> Capture "articleid" Int64 :> Get '[JSON] Article
  :<|> "articles" :> ReqBody '[JSON] Article :> Post '[JSON] Int64
  :<|> "articles" :> "author" :> Capture "authorid" Int64 :> Get '[JSON] [KeyVal Article]
  :<|> "articles" :> "recent" :> Get '[JSON] [(KeyVal User, KeyVal Article)]

usersAPI :: Proxy FullAPI
usersAPI = Proxy :: Proxy FullAPI

fetchUsersHandler :: (Member Database r, Member Cache r) => Int64 -> Eff r User
fetchUsersHandler uid = do
  maybeCachedUser <- fetchCachedUser uid
  case maybeCachedUser of
    Just user -> return user
    Nothing -> do
      maybeUser <- fetchUserDB uid
      case maybeUser of
        Just user -> cacheUser uid user >> return user
        Nothing -> error "Could not find user with that ID"

createUserHandler :: (Member Database r) => User -> Eff r Int64
createUserHandler = createUserDB

fetchArticleHandler :: (Member Database r) => Int64 -> Eff r Article
fetchArticleHandler aid = do
  maybeArticle <- fetchArticleDB aid
  case maybeArticle of
    Just article -> return article
    Nothing -> error "Could not find article with that ID"

createArticleHandler :: (Member Database r) => Article -> Eff r Int64
createArticleHandler = createArticleDB

fetchArticlesByAuthorHandler :: (Member Database r) => Int64 -> Eff r [KeyVal Article]
fetchArticlesByAuthorHandler = fetchArticlesByAuthor

fetchRecentArticlesHandler :: (Member Database r) => Eff r [(KeyVal User, KeyVal Article)]
fetchRecentArticlesHandler = fetchRecentArticles

fullAPIServer :: ((Eff '[Cache, Redis, Database, SqlPersistT (LoggingT IO), IO]) :~> Handler) -> Server FullAPI
fullAPIServer nt =
  enter nt $
    fetchUsersHandler :<|>
    createUserHandler :<|>
    fetchArticleHandler :<|>
    createArticleHandler :<|>
    fetchArticlesByAuthorHandler :<|>
    fetchRecentArticlesHandler

runServer :: IO ()
runServer = do
  pgInfo <- fetchPostgresConnection
  redisInfo <- fetchRedisConnection
  run 8000 (serve usersAPI (fullAPIServer (transformEffToHandler pgInfo redisInfo)))

fetchUserClient :: Int64 -> ClientM User
createUserClient :: User -> ClientM Int64
fetchArticleClient :: Int64 -> ClientM Article
createArticleClient :: Article -> ClientM Int64
fetchArticlesByAuthorClient :: Int64 -> ClientM [KeyVal Article]
fetchRecentArticlesClient :: ClientM [(KeyVal User, KeyVal Article)]
( fetchUserClient             :<|>
  createUserClient            :<|>
  fetchArticleClient          :<|>
  createArticleClient         :<|>
  fetchArticlesByAuthorClient :<|>
  fetchRecentArticlesClient )  = client (Proxy :: Proxy FullAPI)
