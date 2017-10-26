{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Control.Concurrent (killThread)
import Control.Exception.Safe (SomeException)
import Control.Monad.IO.Class (liftIO)
import Control.Monad (forM, void)
import Data.Either (isLeft, isRight)
import Data.Int (Int64)
import Data.Maybe (isJust)
import Data.Time.Clock.POSIX (posixSecondsToUTCTime)
import Database.Persist.Postgresql (fromSqlKey, Key, toSqlKey, entityVal, Entity(..))
import Servant.Client (runClientM, ClientEnv)
import Test.Hspec

import API (fetchUserClient, createUserClient, createArticleClient, fetchArticleClient,
            fetchArticlesByAuthorClient, fetchRecentArticlesClient)
import Cache (RedisInfo)
import Database (PGInfo)
import Monad.App (runAppAction, AppMonad)
import Monad.Cache (fetchCachedUser, deleteCachedUser)
import Monad.Database (fetchUserDB, deleteUserDB, deleteArticleDB, fetchArticleDB, createUserDB, createArticleDB)
import Schema (User(..), Article(..))
import TestUtils (setupTests)
import Types (KeyVal(..), getVal)

main :: IO ()
main = do
  (pgInfo, redisInfo, clientEnv, tid) <- setupTests
  hspec $ before (beforeHook1 clientEnv pgInfo redisInfo) spec1
  hspec $ before (beforeHook2 clientEnv pgInfo redisInfo) $ after (afterHook pgInfo redisInfo) $ spec2
  hspec $ before (beforeHook3 clientEnv pgInfo redisInfo) $ after (afterHook pgInfo redisInfo) $ spec3
  hspec $ before (beforeHook4 clientEnv pgInfo redisInfo) $ after (afterHook4 pgInfo redisInfo) $ spec4
  hspec $ before (beforeHook5 clientEnv pgInfo redisInfo) $ after (afterHook5 pgInfo redisInfo) $ spec5
  hspec $ before (beforeHook6 clientEnv pgInfo redisInfo) $ after (afterHook6 pgInfo redisInfo) $ spec6
  killThread tid 
  return ()

runAppIgnoreError :: String -> PGInfo -> RedisInfo -> AppMonad a -> IO a
runAppIgnoreError msg pgInfo redisInfo action = do
  (result :: Either SomeException a) <- runAppAction pgInfo redisInfo action
  case result of
    Left _ -> error msg
    Right r -> return r

beforeHook1 :: ClientEnv -> PGInfo -> RedisInfo -> IO (Bool, Bool, Bool)
beforeHook1 clientEnv pgInfo redisInfo = do
  (result :: Either SomeException (Bool, Bool, Bool)) <- runAppAction pgInfo redisInfo $ do
    callResult <- liftIO $ runClientM (fetchUserClient 1) clientEnv
    let throwsError = isLeft callResult
    inPG <- isJust <$> fetchUserDB 1
    inRedis <- isJust <$> fetchCachedUser 1
    return (throwsError, inPG, inRedis)
  case result of
    Left _ -> error "Before Hook 1 Failed!"
    Right r -> return r

spec1 :: SpecWith (Bool, Bool, Bool)
spec1 = describe "After fetching on an empty database" $ do
  it "The fetch call should throw an error" $ \(throwsError, _, _) -> throwsError `shouldBe` True
  it "There should be no user in Postgres" $ \(_, inPG, _) -> inPG `shouldBe` False
  it "There should be no user in Redis" $ \(_, _, inRedis) -> inRedis `shouldBe` False

beforeHook2 :: ClientEnv -> PGInfo -> RedisInfo -> IO (Bool, Bool, Int64)
beforeHook2 clientEnv pgInfo redisInfo = runAppIgnoreError "Before Hook 2" pgInfo redisInfo $ do
  userKeyEither <- liftIO $ runClientM (createUserClient testUser1) clientEnv
  case userKeyEither of
    Left _ -> error "DB call failed on spec 2!"
    Right userKey -> do 
      inPG <- isJust <$> fetchUserDB userKey
      inRedis <- isJust <$> fetchCachedUser userKey
      return (inPG, inRedis, userKey)

spec2 :: SpecWith (Bool, Bool, Int64)
spec2 = describe "After creating the user but not fetching" $ do
  it "There should be a user in Postgres" $ \(inPG, _, _) -> inPG `shouldBe` True
  it "There should be no user in Redis" $ \(_, inRedis, _) -> inRedis `shouldBe` False

afterHook :: PGInfo -> RedisInfo -> (Bool, Bool, Int64) -> IO ()
afterHook pgInfo redisInfo (_, _, key) = runAppIgnoreError "After Hook" pgInfo redisInfo $ do
  deleteCachedUser key
  deleteUserDB key

beforeHook3 :: ClientEnv -> PGInfo -> RedisInfo -> IO (Bool, Bool, Int64)
beforeHook3 clientEnv pgInfo redisInfo = runAppIgnoreError "Before Hook 3" pgInfo redisInfo $ do
  userKeyEither <- liftIO $ runClientM (createUserClient testUser1) clientEnv
  case userKeyEither of
    Left _ -> error "DB call failed on spec 3!"
    Right userKey -> do 
      _ <- liftIO $ runClientM (fetchUserClient userKey) clientEnv 
      inPG <- isJust <$> fetchUserDB userKey
      inRedis <- isJust <$> fetchCachedUser userKey
      return (inPG, inRedis, userKey)

spec3 :: SpecWith (Bool, Bool, Int64)
spec3 = describe "After creating the user and fetching" $ do
  it "There should be a user in Postgres" $ \(inPG, _, _) -> inPG `shouldBe` True
  it "There should be a user in Redis" $ \(_, inRedis, _) -> inRedis `shouldBe` True

beforeHook4 :: ClientEnv -> PGInfo -> RedisInfo -> IO (Bool, Bool, Int64, Int64)
beforeHook4 clientEnv pgInfo redisInfo = runAppIgnoreError "Before Hook 4" pgInfo redisInfo $ do
  userKey <- createUserDB testUser2
  articleKeyEither <- liftIO $ runClientM (createArticleClient (testArticle1 userKey)) clientEnv
  case articleKeyEither of
    Left _ -> error "DB call failed on spec 4!"
    Right articleKey -> do
      fetchResult <- liftIO $ runClientM (fetchArticleClient articleKey) clientEnv
      let callSucceeds = isRight fetchResult
      articleInPG <- isJust <$> fetchArticleDB articleKey
      return (callSucceeds, articleInPG, userKey, articleKey)

spec4 :: SpecWith (Bool, Bool, Int64, Int64)
spec4 = describe "After creating and fetching an article" $ do
  it "The fetch call should return a result" $ \(succeeds, _, _, _) -> succeeds `shouldBe` True
  it "The article should be in Postgres" $ \(_, inPG, _, _) -> inPG `shouldBe` True

afterHook4 :: PGInfo -> RedisInfo -> (Bool, Bool, Int64, Int64) -> IO ()
afterHook4 pgInfo redisInfo (_, _, uid, aid) = deleteArtifacts pgInfo redisInfo [uid] [aid]

-- Create 5 articles, three belonging to 1 user and two belonging to another
-- Returned articles should match our tests
beforeHook5 :: ClientEnv -> PGInfo -> RedisInfo -> IO ([Article], [Article], Int64, Int64, [Int64])
beforeHook5 clientEnv pgInfo redisInfo = runAppIgnoreError "Before Hook 5" pgInfo redisInfo $ do
  uid1 <- createUserDB testUser3
  uid2 <- createUserDB testUser4
  articleIds <- mapM createArticleDB
    [ testArticle2 uid1, testArticle3 uid1, testArticle4 uid1
    , testArticle5 uid2, testArticle6 uid2 ]
  firstArticles <- liftIO $ runClientM (fetchArticlesByAuthorClient uid1) clientEnv
  secondArticles <- liftIO $ runClientM (fetchArticlesByAuthorClient uid2) clientEnv
  case (firstArticles, secondArticles) of
    (Right as1, Right as2) -> return (getVal <$> as1, getVal <$> as2, uid1, uid2, articleIds)
    _ -> error "Spec 5 failed!"

-- Two tests, 3, 2
spec5 :: SpecWith ([Article], [Article], Int64, Int64, [Int64])
spec5 = describe "When fetching articles by author ID" $ do
  it "Fetching by the first author should return 3 articles" $ \(firstArticles, _, uid1, _, _) ->
    firstArticles `shouldBe` [testArticle2 uid1, testArticle3 uid1, testArticle4 uid1]
  it "Fetching by the second author should return 2 articles" $ \(_, secondArticles, _, uid2, _) ->
    secondArticles `shouldBe` [testArticle5 uid2, testArticle6 uid2]

afterHook5 :: PGInfo -> RedisInfo -> ([Article], [Article], Int64, Int64, [Int64]) -> IO ()
afterHook5 pgInfo redisInfo (_, _, uid1, uid2, aids) =
  deleteArtifacts pgInfo redisInfo [uid1, uid2] aids

beforeHook6 :: ClientEnv -> PGInfo -> RedisInfo -> IO ([(User, Article)], Int64, Int64, [Int64])
beforeHook6 clientEnv pgInfo redisInfo = runAppIgnoreError "Before Hook 6" pgInfo redisInfo $ do
  uid1 <- createUserDB testUser5
  uid2 <- createUserDB testUser6
  articleIds <- mapM createArticleDB
    [ testArticle7 uid1, testArticle8 uid1, testArticle9 uid1, testArticle10 uid2
    , testArticle11 uid2, testArticle12 uid1, testArticle13 uid2, testArticle14 uid2
    , testArticle15 uid2, testArticle16 uid1, testArticle17 uid1, testArticle18 uid2
    ]
  recentArticles <- liftIO $ runClientM fetchRecentArticlesClient clientEnv
  case recentArticles of
    Right as -> return (valTuple <$> as, uid1, uid2, articleIds)
    _ -> error "Spec 6 failed!"
  where
    valTuple (KeyVal (_, u), KeyVal (_, a)) = (u, a)

spec6 :: SpecWith ([(User, Article)], Int64, Int64, [Int64])
spec6 = describe "When fetching recent articles" $ do
  it "Should fetch exactly the 10 most recent articles" $ \(pairs, uid1, uid2, _) ->
    pairs `shouldBe` mkAllPairs uid1 uid2

afterHook6 ::
  PGInfo -> RedisInfo -> ([(User, Article)], Int64, Int64, [Int64]) -> IO ()
afterHook6 pgInfo redisInfo (_, uid1, uid2, aids) =
  deleteArtifacts pgInfo redisInfo [uid1, uid2] aids

deleteArtifacts :: PGInfo -> RedisInfo -> [Int64] -> [Int64] -> IO ()
deleteArtifacts pgInfo redisInfo users articles = runAppIgnoreError "Deleting artifacts" pgInfo redisInfo  $ do
  void $ forM articles $ \a -> deleteArticleDB a
  void $ forM users $ \u -> do
    deleteCachedUser u
    deleteUserDB u

testUser1 :: User
testUser1 = User
  { userName = "james"
  , userEmail = "james@test.com"
  , userAge = 25
  , userOccupation = "Software Engineer"
  }

testUser2 :: User
testUser2 = User
  { userName = "kate"
  , userEmail = "kate@test.com"
  , userAge = 24
  , userOccupation = "Software Engineer"
  }

testUser3 :: User
testUser3 = User
  { userName = "jeremy"
  , userEmail = "jeremy@test.com"
  , userAge = 23
  , userOccupation = "Teacher"
  }

testUser4 :: User
testUser4 = User
  { userName = "alex"
  , userEmail = "alex@test.com"
  , userAge = 30
  , userOccupation = "Petroleum Engineer"
  }

testUser5 :: User
testUser5 = User
  { userName = "adam"
  , userEmail = "adam@test.com"
  , userAge = 30
  , userOccupation = "Accountant"
  }

testUser6 :: User
testUser6 = User
  { userName = "alexa"
  , userEmail = "alexa@test.com"
  , userAge = 30
  , userOccupation = "Mechanical Engineer"
  }

testArticle1 :: Int64 -> Article
testArticle1 uid = Article
  { articleTitle = "First post"
  , articleBody = "A great description of our first blog post body."
  , articlePublishedTime = posixSecondsToUTCTime 1498914000
  , articleAuthorId = toSqlKey uid
  }

testArticle2 :: Int64 -> Article
testArticle2 uid = Article
  { articleTitle = "Second post"
  , articleBody = "Dummy body description"
  , articlePublishedTime = posixSecondsToUTCTime 1498917600
  , articleAuthorId = toSqlKey uid
  }

testArticle3 :: Int64 -> Article
testArticle3 uid = Article
  { articleTitle = "Third post"
  , articleBody = "Fascinating!"
  , articlePublishedTime = posixSecondsToUTCTime 1498921200
  , articleAuthorId = toSqlKey uid
  }

testArticle4 :: Int64 -> Article
testArticle4 uid = Article
  { articleTitle = "Fourth post"
  , articleBody = "Overall summary of the blog"
  , articlePublishedTime = posixSecondsToUTCTime 1498924800
  , articleAuthorId = toSqlKey uid
  }

testArticle5 :: Int64 -> Article
testArticle5 uid = Article
  { articleTitle = "Fifth post"
  , articleBody = "Description of everything in the universe"
  , articlePublishedTime = posixSecondsToUTCTime 1498928400
  , articleAuthorId = toSqlKey uid
  }

testArticle6 :: Int64 -> Article
testArticle6 uid = Article
  { articleTitle = "Sixth post"
  , articleBody = "A reflection on the events of the past week"
  , articlePublishedTime = posixSecondsToUTCTime 1498932000
  , articleAuthorId = toSqlKey uid
  }

testArticle7 :: Int64 -> Article
testArticle7 uid = Article
  { articleTitle = "Seventh post"
  , articleBody = "A great description of our first blog post body."
  , articlePublishedTime = posixSecondsToUTCTime 1498914000
  , articleAuthorId = toSqlKey uid
  }

testArticle8 :: Int64 -> Article
testArticle8 uid = Article
  { articleTitle = "Eighth post"
  , articleBody = "Dummy body description"
  , articlePublishedTime = posixSecondsToUTCTime 1498917600
  , articleAuthorId = toSqlKey uid
  }

testArticle9 :: Int64 -> Article
testArticle9 uid = Article
  { articleTitle = "Ninth post"
  , articleBody = "Fascinating!"
  , articlePublishedTime = posixSecondsToUTCTime 1498921200
  , articleAuthorId = toSqlKey uid
  }

testArticle10 :: Int64 -> Article
testArticle10 uid = Article
  { articleTitle = "Tenth post"
  , articleBody = "Overall summary of the blog"
  , articlePublishedTime = posixSecondsToUTCTime 1498924800
  , articleAuthorId = toSqlKey uid
  }

testArticle11 :: Int64 -> Article
testArticle11 uid = Article
  { articleTitle = "Eleventh post"
  , articleBody = "Description of everything in the universe"
  , articlePublishedTime = posixSecondsToUTCTime 1498928400
  , articleAuthorId = toSqlKey uid
  }

testArticle12 :: Int64 -> Article
testArticle12 uid = Article
  { articleTitle = "Twelfth post"
  , articleBody = "A reflection on the events of the past week"
  , articlePublishedTime = posixSecondsToUTCTime 1498932000
  , articleAuthorId = toSqlKey uid
  }

testArticle13 :: Int64 -> Article
testArticle13 uid = Article
  { articleTitle = "Thirteenth post"
  , articleBody = "Overall summary of the blog"
  , articlePublishedTime = posixSecondsToUTCTime 1498914001
  , articleAuthorId = toSqlKey uid
  }

testArticle14 :: Int64 -> Article
testArticle14 uid = Article
  { articleTitle = "Fourteenth post"
  , articleBody = "Description of everything in the universe"
  , articlePublishedTime = posixSecondsToUTCTime 1498917601
  , articleAuthorId = toSqlKey uid
  }

testArticle15 :: Int64 -> Article
testArticle15 uid = Article
  { articleTitle = "Fifteenth post"
  , articleBody = "A reflection on the events of the past week"
  , articlePublishedTime = posixSecondsToUTCTime 1498921201
  , articleAuthorId = toSqlKey uid
  }

testArticle16 :: Int64 -> Article
testArticle16 uid = Article
  { articleTitle = "Sixteenth post"
  , articleBody = "Overall summary of the blog"
  , articlePublishedTime = posixSecondsToUTCTime 1498924801
  , articleAuthorId = toSqlKey uid
  }

testArticle17 :: Int64 -> Article
testArticle17 uid = Article
  { articleTitle = "Seventeenth post"
  , articleBody = "Description of everything in the universe"
  , articlePublishedTime = posixSecondsToUTCTime 1498928401
  , articleAuthorId = toSqlKey uid
  }

testArticle18 :: Int64 -> Article
testArticle18 uid = Article
  { articleTitle = "Eighteenth post"
  , articleBody = "A reflection on the events of the past week"
  , articlePublishedTime = posixSecondsToUTCTime 1498932001
  , articleAuthorId = toSqlKey uid
  }

mkAllPairs :: Int64 -> Int64 -> [(User, Article)]
mkAllPairs uid1 uid2 =
  [ (testUser6, testArticle18 uid2)
  , (testUser5, testArticle12 uid1)
  , (testUser5, testArticle17 uid1)
  , (testUser6, testArticle11 uid2)
  , (testUser5, testArticle16 uid1)
  , (testUser6, testArticle10 uid2)
  , (testUser6, testArticle15 uid2)
  , (testUser5, testArticle9 uid1)
  , (testUser6, testArticle14 uid2)
  , (testUser5, testArticle8 uid1)
  ]
