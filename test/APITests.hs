{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Control.Concurrent (killThread, MVar, readMVar, swapMVar)
import Control.Exception.Safe (SomeException)
import Control.Monad.IO.Class (liftIO)
import Control.Monad (forM, void)
import Control.Monad.State (runStateT)
import Data.Either (isLeft, isRight)
import Data.Int (Int64)
import Data.Map (empty)
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
import TestMonad (TestMonad(..), UserMap, ArticleMap)
import TestUtils (setupTests')
import Types (KeyVal(..), getVal)

main :: IO ()
main = do
  (clientEnv, dbMap, tid) <- setupTests'
  hspec $ before (beforeHook1 clientEnv dbMap) spec1
  hspec $ before (beforeHook2 clientEnv dbMap) spec2
  hspec $ before (beforeHook3 clientEnv dbMap) spec3
  hspec $ before (beforeHook4 clientEnv dbMap) spec4
  hspec $ before (beforeHook5 clientEnv dbMap) spec5
  hspec $ before (beforeHook6 clientEnv dbMap) spec6
  killThread tid 
  return ()

runAppIgnoreError :: String -> PGInfo -> RedisInfo -> AppMonad a -> IO a
runAppIgnoreError msg pgInfo redisInfo action = do
  (result :: Either SomeException a) <- runAppAction pgInfo redisInfo action
  case result of
    Left _ -> error msg
    Right r -> return r

runTestMonad :: MVar (UserMap, ArticleMap, UserMap) -> TestMonad a -> IO a
runTestMonad mapVar (TestMonad action) = do
  currentState <- readMVar mapVar
  (result, newMap) <- runStateT action currentState
  swapMVar mapVar newMap
  return result

beforeHook1 :: ClientEnv -> MVar (UserMap, ArticleMap, UserMap) -> IO (Bool, Bool, Bool)
beforeHook1 clientEnv mapVar = do
  callResult <- runClientM (fetchUserClient 1) clientEnv
  let throwsError = isLeft callResult
  (inPG, inRedis) <- runTestMonad mapVar $ do
    inPG <- isJust <$> fetchUserDB 1
    inRedis <- isJust <$> fetchCachedUser 1
    return (inPG, inRedis)
  return (throwsError, inPG, inRedis)

spec1 :: SpecWith (Bool, Bool, Bool)
spec1 = describe "After fetching on an empty database" $ do
  it "The fetch call should throw an error" $ \(throwsError, _, _) -> throwsError `shouldBe` True
  it "There should be no user in Postgres" $ \(_, inPG, _) -> inPG `shouldBe` False
  it "There should be no user in Redis" $ \(_, _, inRedis) -> inRedis `shouldBe` False

beforeHook2 :: ClientEnv -> MVar (UserMap, ArticleMap, UserMap) -> IO (Bool, Bool, Int64)
beforeHook2 clientEnv mapVar = do
  userKeyEither <- runClientM (createUserClient testUser1) clientEnv
  case userKeyEither of
    Left _ -> error "DB call failed on spec 2!"
    Right userKey -> runTestMonad mapVar $ do 
      inPG <- isJust <$> fetchUserDB userKey
      inRedis <- isJust <$> fetchCachedUser userKey
      return (inPG, inRedis, userKey)

spec2 :: SpecWith (Bool, Bool, Int64)
spec2 = describe "After creating the user but not fetching" $ do
  it "There should be a user in Postgres" $ \(inPG, _, _) -> inPG `shouldBe` True
  it "There should be no user in Redis" $ \(_, inRedis, _) -> inRedis `shouldBe` False

beforeHook3 :: ClientEnv -> MVar (UserMap, ArticleMap, UserMap) -> IO (Bool, Bool, Int64)
beforeHook3 clientEnv mapVar = do
  userKeyEither <- runClientM (createUserClient testUser1) clientEnv
  case userKeyEither of
    Left _ -> error "DB call failed on spec 3!"
    Right userKey -> do 
      _ <- liftIO $ runClientM (fetchUserClient userKey) clientEnv 
      (inPG, inRedis) <- runTestMonad mapVar $ do
        inPG <- isJust <$> fetchUserDB userKey
        inRedis <- isJust <$> fetchCachedUser userKey
        return (inPG, inRedis)
      return (inPG, inRedis, userKey)

spec3 :: SpecWith (Bool, Bool, Int64)
spec3 = describe "After creating the user and fetching" $ do
  it "There should be a user in Postgres" $ \(inPG, _, _) -> inPG `shouldBe` True
  it "There should be a user in Redis" $ \(_, inRedis, _) -> inRedis `shouldBe` True

beforeHook4 :: ClientEnv -> MVar (UserMap, ArticleMap, UserMap) -> IO (Bool, Bool, Int64, Int64)
beforeHook4 clientEnv mapVar = do
  userKey <- runTestMonad mapVar $ createUserDB testUser2
  articleKeyEither <- runClientM (createArticleClient (testArticle1 userKey)) clientEnv
  case articleKeyEither of
    Left _ -> error "DB call failed on spec 4!"
    Right articleKey -> do
      fetchResult <- runClientM (fetchArticleClient articleKey) clientEnv
      let callSucceeds = isRight fetchResult
      articleInPG <- runTestMonad mapVar $ (isJust <$> fetchArticleDB articleKey)
      return (callSucceeds, articleInPG, userKey, articleKey)

spec4 :: SpecWith (Bool, Bool, Int64, Int64)
spec4 = describe "After creating and fetching an article" $ do
  it "The fetch call should return a result" $ \(succeeds, _, _, _) -> succeeds `shouldBe` True
  it "The article should be in Postgres" $ \(_, inPG, _, _) -> inPG `shouldBe` True

-- Create 5 articles, three belonging to 1 user and two belonging to another
-- Returned articles should match our tests
beforeHook5 :: ClientEnv -> MVar (UserMap, ArticleMap, UserMap) -> IO ([Article], [Article], Int64, Int64, [Int64])
beforeHook5 clientEnv mapVar = do
  (uid1, uid2, articleIds) <- runTestMonad mapVar $ do
    uid1 <- createUserDB testUser3
    uid2 <- createUserDB testUser4
    articleIds <- mapM createArticleDB
      [ testArticle2 uid1, testArticle3 uid1, testArticle4 uid1
      , testArticle5 uid2, testArticle6 uid2 ]
    return (uid1, uid2, articleIds)
  firstArticles <- runClientM (fetchArticlesByAuthorClient uid1) clientEnv
  secondArticles <- runClientM (fetchArticlesByAuthorClient uid2) clientEnv
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

beforeHook6 :: ClientEnv -> MVar (UserMap, ArticleMap, UserMap) -> IO ([(User, Article)], Int64, Int64, [Int64])
beforeHook6 clientEnv mapVar = do
  (uid1, uid2, articleIds) <- runTestMonad mapVar $ do
    uid1 <- createUserDB testUser5
    uid2 <- createUserDB testUser6
    articleIds <- mapM createArticleDB
      [ testArticle7 uid1, testArticle8 uid1, testArticle9 uid1, testArticle10 uid2
      , testArticle11 uid2, testArticle12 uid1, testArticle13 uid2, testArticle14 uid2
      , testArticle15 uid2, testArticle16 uid1, testArticle17 uid1, testArticle18 uid2
      ]
    return (uid1, uid2, articleIds)
  recentArticles <- runClientM fetchRecentArticlesClient clientEnv
  case recentArticles of
    Right as -> return (valTuple <$> as, uid1, uid2, articleIds)
    _ -> error "Spec 6 failed!"
  where
    valTuple (KeyVal (_, u), KeyVal (_, a)) = (u, a)

spec6 :: SpecWith ([(User, Article)], Int64, Int64, [Int64])
spec6 = describe "When fetching recent articles" $ do
  it "Should fetch exactly the 10 most recent articles" $ \(pairs, uid1, uid2, _) ->
    pairs `shouldBe` mkAllPairs uid1 uid2

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
  , articlePublishedTime = posixSecondsToUTCTime 1497914000
  , articleAuthorId = toSqlKey uid
  }

testArticle2 :: Int64 -> Article
testArticle2 uid = Article
  { articleTitle = "Second post"
  , articleBody = "Dummy body description"
  , articlePublishedTime = posixSecondsToUTCTime 1497917600
  , articleAuthorId = toSqlKey uid
  }

testArticle3 :: Int64 -> Article
testArticle3 uid = Article
  { articleTitle = "Third post"
  , articleBody = "Fascinating!"
  , articlePublishedTime = posixSecondsToUTCTime 1497921200
  , articleAuthorId = toSqlKey uid
  }

testArticle4 :: Int64 -> Article
testArticle4 uid = Article
  { articleTitle = "Fourth post"
  , articleBody = "Overall summary of the blog"
  , articlePublishedTime = posixSecondsToUTCTime 1497924800
  , articleAuthorId = toSqlKey uid
  }

testArticle5 :: Int64 -> Article
testArticle5 uid = Article
  { articleTitle = "Fifth post"
  , articleBody = "Description of everything in the universe"
  , articlePublishedTime = posixSecondsToUTCTime 1497928400
  , articleAuthorId = toSqlKey uid
  }

testArticle6 :: Int64 -> Article
testArticle6 uid = Article
  { articleTitle = "Sixth post"
  , articleBody = "A reflection on the events of the past week"
  , articlePublishedTime = posixSecondsToUTCTime 1497932000
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
