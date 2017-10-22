module Monad.Database where

import Data.Int (Int64)

import Schema

class (Monad m) => MonadDatabase m where
  fetchUserDB :: Int64 -> m (Maybe User) 
  createUserDB :: User -> m Int64 
  deleteUserDB :: Int64 -> m ()
  fetchArticleDB :: Int64 -> m (Maybe Article)
  createArticleDB :: Article -> m Int64
  deleteArticleDB :: Int64 -> m ()
  fetchArticlesByAuthor :: Int64 -> m [(Int64, Article)]
  fetchRecentArticles :: m [((Int64, User), (Int64, Article))]
