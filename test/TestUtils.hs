module TestUtils where

import Control.Concurrent (forkIO, ThreadId, threadDelay)
import Control.Monad.Logger (runStdoutLoggingT)
import Control.Monad.Reader (runReaderT)
import Database.Persist.Postgresql (withPostgresqlConn, runMigrationSilent)

import API (runServer)
import Database (PGInfo, fetchPostgresConnection, RedisInfo, fetchRedisConnection)
import Schema (migrateAll)

setupTests :: IO (PGInfo, RedisInfo, ThreadId)
setupTests = do
  pgInfo <- fetchPostgresConnection
  redisInfo <- fetchRedisConnection
  runStdoutLoggingT $ withPostgresqlConn pgInfo $ \dbConn ->
    runReaderT (runMigrationSilent migrateAll) dbConn
  tid <- forkIO runServer
  threadDelay 1000000
  return (pgInfo, redisInfo, tid)
