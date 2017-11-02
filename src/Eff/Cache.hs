{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}

module Eff.Cache where

import Control.Monad (void)
import Data.ByteString.Char8 (pack, unpack)
import Database.Redis (Redis, get, setex, del)
import Control.Monad.Freer (Member, Eff, send, runNat)
import Data.Int (Int64)

import Schema

data Cache a where
  CacheUser :: Int64 -> User -> Cache ()
  FetchCachedUser :: Int64 -> Cache (Maybe User)
  DeleteCachedUser :: Int64 -> Cache ()

cacheUser :: (Member Cache r) => Int64 -> User -> Eff r ()
cacheUser uid user = send $ CacheUser uid user

fetchCachedUser :: (Member Cache r) => Int64 -> Eff r (Maybe User)
fetchCachedUser = send . FetchCachedUser

deleteCachedUser :: (Member Cache r) => Int64 -> Eff r ()
deleteCachedUser = send . DeleteCachedUser

runCache :: (Member Redis r) => Eff (Cache ': r) a -> Eff r a
runCache = runNat cacheToRedis
  where
    cacheToRedis :: Cache a -> Redis a
    cacheToRedis (CacheUser uid user) = void $ setex (pack . show $ uid) 3600 (pack . show $ user)
    cacheToRedis (FetchCachedUser uid) = do
      result <- get (pack . show $ uid)
      case result of
        Right (Just userString) -> return $ Just (read . unpack $ userString)
        _ -> return Nothing
    cacheToRedis (DeleteCachedUser uid) = void $ del [pack . show $ uid] 
