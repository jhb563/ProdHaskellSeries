module Types where

import Database.Persist.Postgresql (ConnectionString)
import Database.Redis (ConnectInfo)

type PGInfo = ConnectionString
type RedisInfo = ConnectInfo
