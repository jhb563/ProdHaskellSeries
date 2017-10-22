{-# OPTIONS_GHC -fno-warn-orphans #-}

module Cache where

import Control.Monad (void)
import Data.ByteString.Char8 (pack, unpack)
import Database.Redis (Redis, get, setex, del, defaultConnectInfo, ConnectInfo)

import Monad.Cache (MonadCache(..))

type RedisInfo = ConnectInfo

fetchRedisConnection :: IO RedisInfo
fetchRedisConnection = return defaultConnectInfo

instance MonadCache Redis where
  cacheUser uid user = void $ setex (pack . show $ uid) 3600 (pack . show $ user)
  fetchCachedUser uid = do
    result <- get (pack . show $ uid)
    case result of
      Right (Just userString) -> return $ Just (read . unpack $ userString)
      _ -> return Nothing
  deleteCachedUser uid = void $ del [pack . show $ uid]
