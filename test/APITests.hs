{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Concurrent (killThread)
import Data.Int (Int64)
import Data.Maybe (isJust)
import Database.Persist.Postgresql (fromSqlKey)
import Network.HTTP.Client (newManager)
import Network.HTTP.Client.TLS (tlsManagerSettings)
import Servant.Client (ClientEnv(..), runClientM)
import Servant.Common.BaseUrl (parseBaseUrl)
import Test.Hspec

import API (fetchUserClient, createUserClient)
import Database (PGInfo, RedisInfo, fetchUserPG, deleteUserPG, fetchUserRedis, deleteUserCache)
import Schema (User(..))
import TestUtils (setupTests)

main :: IO ()
main = do
  (pgInfo, redisInfo, tid) <- setupTests
  mgr <- newManager tlsManagerSettings
  baseUrl <- parseBaseUrl "http://127.0.0.1:8000"
  let clientEnv = ClientEnv mgr baseUrl
  hspec $ before (beforeHook1 clientEnv pgInfo redisInfo) spec1
  hspec $ before (beforeHook2 clientEnv pgInfo redisInfo) $ after (afterHook pgInfo redisInfo) $ spec2
  hspec $ before (beforeHook3 clientEnv pgInfo redisInfo) $ after (afterHook pgInfo redisInfo) $ spec3
  killThread tid 
  return ()

beforeHook1 :: ClientEnv -> PGInfo -> RedisInfo -> IO (Bool, Bool)
beforeHook1 clientEnv pgInfo redisInfo = do
  _ <- runClientM (fetchUserClient 1) clientEnv
  inPG <- isJust <$> fetchUserPG pgInfo 1
  inRedis <- isJust <$> fetchUserRedis redisInfo 1
  return (inPG, inRedis)

spec1 :: SpecWith (Bool, Bool)
spec1 = describe "After fetching on an empty database" $ do
  it "There should be no user in Postgres" $ \(inPG, _) -> inPG `shouldBe` False
  it "There should be no user in Redis" $ \(_, inRedis) -> inRedis `shouldBe` False

beforeHook2 :: ClientEnv -> PGInfo -> RedisInfo -> IO (Bool, Bool, Int64)
beforeHook2 clientEnv pgInfo redisInfo = do
  userKeyEither <- runClientM (createUserClient testUser) clientEnv
  case userKeyEither of
    Left _ -> error "DB call failed on spec 2!"
    Right userKey -> do 
      inPG <- isJust <$> fetchUserPG pgInfo userKey
      inRedis <- isJust <$> fetchUserRedis redisInfo userKey
      return (inPG, inRedis, userKey)

spec2 :: SpecWith (Bool, Bool, Int64)
spec2 = describe "After creating the user but not fetching" $ do
  it "There should be a user in Postgres" $ \(inPG, _, _) -> inPG `shouldBe` True
  it "There should be no user in Redis" $ \(_, inRedis, _) -> inRedis `shouldBe` False

afterHook :: PGInfo -> RedisInfo -> (Bool, Bool, Int64) -> IO ()
afterHook pgInfo redisInfo (_, _, key) = do
  deleteUserCache redisInfo key
  deleteUserPG pgInfo key

beforeHook3 :: ClientEnv -> PGInfo -> RedisInfo -> IO (Bool, Bool, Int64)
beforeHook3 clientEnv pgInfo redisInfo = do
  userKeyEither <- runClientM (createUserClient testUser) clientEnv
  case userKeyEither of
    Left _ -> error "DB call failed on spec 3!"
    Right userKey -> do 
      _ <- runClientM (fetchUserClient userKey) clientEnv 
      inPG <- isJust <$> fetchUserPG pgInfo userKey
      inRedis <- isJust <$> fetchUserRedis redisInfo userKey
      return (inPG, inRedis, userKey)

spec3 :: SpecWith (Bool, Bool, Int64)
spec3 = describe "After creating the user and fetching" $ do
  it "There should be a user in Postgres" $ \(inPG, _, _) -> inPG `shouldBe` True
  it "There should be a user in Redis" $ \(_, inRedis, _) -> inRedis `shouldBe` True

testUser :: User
testUser = User
  { userName = "james"
  , userEmail = "james@test.com"
  , userAge = 25
  , userOccupation = "Software Engineer"
  }

