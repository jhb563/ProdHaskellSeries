{-# LANGUAGE OverloadedStrings #-}

module Database where

import           Control.Monad (void)
import           Control.Monad.Logger (runStdoutLoggingT, MonadLogger, LoggingT)
import           Control.Monad.Reader (runReaderT)
import           Control.Monad.IO.Class (MonadIO)
import           Data.ByteString.Char8 (pack, unpack)
import           Data.Int (Int64)
import           Database.Persist (get, insert, delete)
import           Database.Persist.Sql (fromSqlKey, toSqlKey)
import           Database.Persist.Postgresql (ConnectionString, withPostgresqlConn, runMigration, SqlPersistT)
import           Database.Redis (ConnectInfo, connect, Redis, runRedis, defaultConnectInfo, setex)
import qualified Database.Redis as Redis

import           Schema

type PGInfo = ConnectionString
type RedisInfo = ConnectInfo

localConnString :: PGInfo
localConnString = "host=127.0.0.1 port=5432 user=postgres dbname=postgres"

-- This is IO since in a real application we'd want to configure it.
fetchPostgresConnection :: IO PGInfo
fetchPostgresConnection = return localConnString

fetchRedisConnection :: IO RedisInfo
fetchRedisConnection = return defaultConnectInfo

runAction :: PGInfo -> SqlPersistT (LoggingT IO) a -> IO a
runAction connectionString action = 
  runStdoutLoggingT $ withPostgresqlConn connectionString $ \backend ->
    runReaderT action backend

migrateDB :: PGInfo -> IO ()
migrateDB connString = runAction connString (runMigration migrateAll)

fetchUserPG :: PGInfo -> Int64 -> IO (Maybe User)
fetchUserPG connString uid = runAction connString (get (toSqlKey uid))

createUserPG :: PGInfo -> User -> IO Int64
createUserPG connString user = fromSqlKey <$> runAction connString (insert user)

deleteUserPG :: PGInfo -> Int64 -> IO ()
deleteUserPG connString uid = runAction connString (delete userKey)
  where
    userKey :: Key User
    userKey = toSqlKey uid

runRedisAction :: RedisInfo -> Redis a -> IO a
runRedisAction redisInfo action = do
  connection <- connect redisInfo
  runRedis connection action

cacheUser :: RedisInfo -> Int64 -> User -> IO ()
cacheUser redisInfo uid user = runRedisAction redisInfo $ void $ setex (pack . show $ uid) 3600 (pack . show $ user)

fetchUserRedis :: RedisInfo -> Int64 -> IO (Maybe User)
fetchUserRedis redisInfo uid = runRedisAction redisInfo $ do
  result <- Redis.get (pack . show $ uid)
  case result of
    Right (Just userString) -> return $ Just (read . unpack $ userString)
    _ -> return Nothing
