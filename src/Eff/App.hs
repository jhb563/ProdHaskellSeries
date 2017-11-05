{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}

module Eff.App where

import Control.Monad.Except (throwError)
import Control.Monad.Freer (Eff, runM)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Logger (LoggingT)
import Database.Persist.Sql (SqlPersistT)
import Database.Redis (Redis)
import Servant.Server (Handler(..), (:~>)(..))

import Cache (RedisInfo)
import Database (PGInfo)
import Eff.Cache (Cache, runCache, runRedisAction)
import Eff.Database (Database, runDatabase, runSqlPersist)
import Errors (runWithServantHandler)

transformEffToHandler :: PGInfo -> RedisInfo -> (Eff '[Cache, Redis, Database, SqlPersistT (LoggingT IO), IO]) :~> Handler
transformEffToHandler pgInfo redisInfo = NT $ \action -> do
  let ioAct = (runSqlPersist pgInfo . runDatabase . runRedisAction redisInfo . runCache) action
  result <- liftIO (runWithServantHandler (runM ioAct))
  Handler $ either throwError return result
