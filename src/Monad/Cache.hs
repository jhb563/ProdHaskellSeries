module Monad.Cache where

import Data.Int (Int64)

import Schema

class (Monad m) => MonadCache m where
  cacheUser :: Int64 -> User -> m ()
  fetchCachedUser :: Int64 -> m (Maybe User)
  deleteCachedUser :: Int64 -> m ()
