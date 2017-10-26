{-# LANGUAGE OverloadedStrings #-}

module Types where

import Data.Aeson (ToJSON(..), FromJSON(..), (.=), object, (.:), withObject)
import Data.Int (Int64)

newtype KeyVal a = KeyVal (Int64, a)

getKey :: KeyVal a -> Int64
getKey (KeyVal (k, _)) = k

getVal :: KeyVal a -> a
getVal (KeyVal (_, v)) = v

instance (ToJSON a) => ToJSON (KeyVal a) where
  toJSON (KeyVal (key, val)) = object
    [ "key" .= key
    , "value" .= val
    ]

instance (FromJSON a) => FromJSON (KeyVal a) where
  parseJSON = withObject "Key Val Item" $ \o -> do
    key <- o .: "key"
    val <- o .: "value"
    return $ KeyVal (key, val)
