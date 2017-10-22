{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE OverloadedStrings          #-}

module Schema where

import           Data.Aeson (ToJSON, toJSON, object, (.=), FromJSON, parseJSON, (.:), withObject)
import           Data.Time (UTCTime)
import           Database.Persist.Sql (fromSqlKey, toSqlKey)
import qualified Database.Persist.TH as PTH
import           Data.Text (Text)

PTH.share [PTH.mkPersist PTH.sqlSettings, PTH.mkMigrate "migrateAll"] [PTH.persistLowerCase|
  User sql=users
    name Text
    email Text
    age Int
    occupation Text
    UniqueEmail email
    deriving Show Read Eq

  Article sql=articles
    title Text
    body Text
    publishedTime UTCTime
    authorId UserId
    UniqueTitle title
    deriving Show Read Eq
|]

instance ToJSON User where
  toJSON user = object
    [ "name" .= userName user
    , "email" .= userEmail user
    , "age" .= userAge user
    , "occupation" .= userOccupation user
    ]

instance FromJSON User where
  parseJSON = withObject "User" $ \o -> do
    uName <- o .: "name"
    uEmail <- o .: "email"
    uAge <- o .: "age"
    uOccupation <- o .: "occupation"
    return User
      { userName = uName
      , userEmail = uEmail
      , userAge = uAge
      , userOccupation = uOccupation
      }

instance ToJSON Article where
  toJSON article = object
    [ "title" .= articleTitle article
    , "body" .= articleBody article
    , "publishedTime" .= articlePublishedTime article
    , "authorId" .= fromSqlKey (articleAuthorId article)
    ]

instance FromJSON Article where
  parseJSON = withObject "Article" $ \o -> do
    aTitle <- o .: "title"
    aBody <- o .: "body"
    aPublishedTime <- o .: "publishedTime"
    aAuthorId <- o .: "authorId"
    return Article
      { articleTitle = aTitle
      , articleBody = aBody
      , articlePublishedTime = aPublishedTime
      , articleAuthorId = toSqlKey aAuthorId
      }
