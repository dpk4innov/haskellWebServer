{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}


module Database where

import           Control.Monad.Logger
import           Data.Int (Int64)
import           Database.Persist
import           Database.Persist.Postgresql
import           Data.Text
import           Data.String.Interpolate
import           Config
import           Schema




type PGInfo = ConnectionString

localConnString :: DBConfig ->  PGInfo
localConnString DBConfig {..} = [i|host=#{dbHost} port=#{dbPort} user=#{dbUsername} dbname=#{dbName} password=#{dbPassword}|]

runAction :: PGInfo -> SqlPersistT (LoggingT IO) a -> IO a
runAction connectionString action = 
  runStdoutLoggingT  $ filterLogger logFilter $ withPostgresqlConn connectionString $ \backend ->
    runSqlConn  action backend

migrateDB :: PGInfo -> IO ()
migrateDB connString = runAction connString (runMigration migrateAll)

logFilter :: a -> LogLevel -> Bool
logFilter _ LevelError     = True
logFilter _ LevelWarn      = True
logFilter _ LevelInfo      = True
logFilter _ LevelDebug     = True
logFilter _ (LevelOther _) = False


fetchEmployeeQ :: PGInfo -> Int64 -> IO (Maybe Employee)
fetchEmployeeQ connString uid = runAction connString (get (toSqlKey uid))

createEmployeeQ :: PGInfo -> Employee -> IO Int64
createEmployeeQ connString emp = fromSqlKey <$> runAction connString (insert emp)

selectByName :: PGInfo -> Maybe Text -> IO ([Entity Employee])
selectByName connString empN = 
    case empN of
        Just n -> runAction connString (selectList [EmployeeName ==. n ] [])
        _ -> return []

