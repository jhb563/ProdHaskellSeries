{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}

module API where

import           Data.Int (Int64)
import           Data.Proxy (Proxy(..))
import           Network.Wai.Handler.Warp (run)
import           Servant.API
import           Servant.Client
import           Servant.Server

import           Cache (fetchRedisConnection, RedisInfo)
import           Database (fetchPostgresConnection, PGInfo)
import           Monad.App (AppMonad, transformAppToHandler)
import           Monad.Cache (MonadCache(..))
import           Monad.Database (MonadDatabase(..))
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

fetchUsersHandler :: Int64 -> AppMonad User
fetchUsersHandler uid = do
  maybeCachedUser <- fetchCachedUser uid
  case maybeCachedUser of
    Just user -> return user
    Nothing -> do
      maybeUser <- fetchUserDB uid
      case maybeUser of
        Just user -> cacheUser uid user >> return user
        Nothing -> error "Could not find user with that ID"

createUserHandler :: User -> AppMonad Int64
createUserHandler = createUserDB

fetchArticleHandler :: Int64 -> AppMonad Article
fetchArticleHandler aid = do
  maybeArticle <- fetchArticleDB aid
  case maybeArticle of
    Just article -> return article
    Nothing -> error "Could not find article with that ID"

createArticleHandler :: Article -> AppMonad Int64
createArticleHandler = createArticleDB

fetchArticlesByAuthorHandler :: Int64 -> AppMonad [KeyVal Article]
fetchArticlesByAuthorHandler = fetchArticlesByAuthor

fetchRecentArticlesHandler :: AppMonad [(KeyVal User, KeyVal Article)]
fetchRecentArticlesHandler = fetchRecentArticles

fullAPIServer :: PGInfo -> RedisInfo -> Server FullAPI
fullAPIServer pgInfo redisInfo =
  enter (transformAppToHandler pgInfo redisInfo) $
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
  run 8000 (serve usersAPI (fullAPIServer pgInfo redisInfo))

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
