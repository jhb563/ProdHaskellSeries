module Main where

import Database (localConnString, migrateDB)

main :: IO ()
main = migrateDB localConnString
