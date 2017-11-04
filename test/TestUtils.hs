{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}

module TestUtils where

import Control.Concurrent (forkIO, ThreadId, threadDelay, newMVar, MVar)
import Control.Monad.Freer (Eff)
import Control.Monad.Logger (runStdoutLoggingT)
import Control.Monad.Reader (runReaderT)
import Control.Monad.State (StateT)
import qualified Data.Map as Map
import Database.Persist.Postgresql (withPostgresqlConn, runMigrationSilent)
import Network.HTTP.Client (newManager)
import Network.HTTP.Client.TLS (tlsManagerSettings)
import Network.Wai.Handler.Warp (run)
import Servant.API
import Servant.Client (ClientEnv(..), parseBaseUrl)
import Servant.Server (serve, Server, (:~>), Handler, enter)

import API
import Cache (RedisInfo, fetchRedisConnection)
import Database (PGInfo, fetchPostgresConnection)
import Eff.Cache (Cache)
import Eff.Database (Database)
import Schema (migrateAll)
import TestEff (transformTestEffToHandler)
import TestMonad (transformTestToHandler, TestMonad, UserMap, ArticleMap)

setupTests :: IO (PGInfo, RedisInfo, ClientEnv, ThreadId)
setupTests = do
  pgInfo <- fetchPostgresConnection
  redisInfo <- fetchRedisConnection
  mgr <- newManager tlsManagerSettings
  baseUrl <- parseBaseUrl "http://127.0.0.1:8000"
  let clientEnv = ClientEnv mgr baseUrl
  runStdoutLoggingT $ withPostgresqlConn pgInfo $ \dbConn ->
    runReaderT (runMigrationSilent migrateAll) dbConn
  tid <- forkIO runServer
  threadDelay 1000000
  return (pgInfo, redisInfo, clientEnv, tid)

testAPIServer :: (Eff '[Cache, Database, StateT (UserMap, ArticleMap, UserMap) IO] :~> Handler) -> Server FullAPI
testAPIServer nt =
  enter nt $
    fetchUsersHandler :<|>
    createUserHandler :<|>
    fetchArticleHandler :<|>
    createArticleHandler :<|>
    fetchArticlesByAuthorHandler :<|>
    fetchRecentArticlesHandler

setupTests' :: IO (ClientEnv, MVar (UserMap, ArticleMap, UserMap), ThreadId)
setupTests' = do
  mgr <- newManager tlsManagerSettings
  baseUrl <- parseBaseUrl "http://127.0.0.1:8000"
  let clientEnv = ClientEnv mgr baseUrl
  let initialMap = (Map.empty, Map.empty, Map.empty)
  mapRef <- newMVar initialMap
  tid <- forkIO $
    run 8000 (serve usersAPI (testAPIServer (transformTestEffToHandler mapRef)))
  threadDelay 1000000
  return (clientEnv, mapRef, tid)
