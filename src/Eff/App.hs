{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}

module Eff.App where

import Control.Exception.Safe (handleAny, SomeException, Exception)
import Control.Monad.Except (throwError)
import Control.Monad.Freer (Eff, runM)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Logger (LoggingT)
import Data.ByteString.Lazy.Char8 (pack)
import Database.Persist.Sql (SqlPersistT)
import Database.Redis (Redis)
import Servant.Server (Handler(..), (:~>)(..), ServantErr(..), err500)

import Cache (RedisInfo)
import Database (PGInfo)
import Eff.Cache (Cache, runCache, runRedisAction)
import Eff.Database (Database, runDatabase, runSqlPersist)

transformEffToHandler :: PGInfo -> RedisInfo -> (Eff '[Cache, Redis, Database, SqlPersistT (LoggingT IO), IO]) :~> Handler
transformEffToHandler pgInfo redisInfo = NT $ \action -> do
  let ioAct = (runSqlPersist pgInfo . runDatabase . runRedisAction redisInfo . runCache) action
  result <- liftIO (handleAny handler (runRight ioAct))
  Handler $ either throwError return result
  where
    handler :: SomeException -> IO (Either ServantErr a)
    handler e = return $ Left $ err500 { errBody = pack (show e)}

    runRight :: (Exception e) => Eff '[IO] a -> IO (Either e a)
    runRight action = do
      result <- runM action
      return $ Right result
