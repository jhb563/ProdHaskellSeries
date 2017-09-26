{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}

module API where

import           Control.Monad.IO.Class (liftIO)
import           Control.Monad.Trans.Except (throwE)
import           Data.Int (Int64)
import           Data.Proxy (Proxy(..))
import           Database.Persist (Key, Entity)
import           Network.Wai.Handler.Warp (run)
import           Servant.API
import           Servant.Client
import           Servant.Server

import           Database (fetchUserPG, createUserPG, fetchPostgresConnection, PGInfo, RedisInfo,
                           fetchUserRedis, cacheUser, fetchRedisConnection)
import           Schema

type UsersAPI = 
       "users" :> Capture "userid" Int64 :> Get '[JSON] User
  :<|> "users" :> ReqBody '[JSON] User :> Post '[JSON] Int64

usersAPI :: Proxy UsersAPI
usersAPI = Proxy :: Proxy UsersAPI

fetchUsersHandler :: PGInfo -> RedisInfo -> Int64 -> Handler User
fetchUsersHandler pgInfo redisInfo uid = do
  maybeCachedUser <- liftIO $ fetchUserRedis redisInfo uid
  case maybeCachedUser of
    Just user -> return user
    Nothing -> do
      maybeUser <- liftIO $ fetchUserPG pgInfo uid
      case maybeUser of
        Just user -> liftIO (cacheUser redisInfo uid user) >> return user
        Nothing -> Handler $ (throwE $ err401 { errBody = "Could not find user with that ID" })

createUserHandler :: PGInfo -> User -> Handler Int64
createUserHandler pgInfo user = liftIO $ createUserPG pgInfo user

usersServer :: PGInfo -> RedisInfo -> Server UsersAPI
usersServer pgInfo redisInfo =
  (fetchUsersHandler pgInfo redisInfo) :<|>
  (createUserHandler pgInfo)


runServer :: IO ()
runServer = do
  pgInfo <- fetchPostgresConnection
  redisInfo <- fetchRedisConnection
  run 8000 (serve usersAPI (usersServer pgInfo redisInfo))
