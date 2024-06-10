{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RecordWildCards #-}




module Server where

import           Control.Monad.IO.Class (liftIO)
import           Control.Monad.Trans.Except (throwE)
import           Data.Int (Int64)
import           Data.Proxy (Proxy(..))
import           Network.Wai.Logger       (withStdoutLogger)
import           Network.Wai.Handler.Warp (runSettings,defaultSettings,setLogger,setPort)
import           Servant.API
import           Servant.Server
import           Data.Text
import           Database.Persist.Types
import           Config
import           Database
import           Schema
import           Control.Exception.Safe hiding (Handler)
import           Data.ByteString.Lazy.UTF8 as BLU



type EmployeesAPI = 
       "employees" :> Capture "employeeid" Int64 :> Get '[JSON] Employee
       :<|> "employees" :> QueryParam "name" Text :> Get '[JSON] [Employee]
  :<|> "employees" :> ReqBody '[JSON] Employee :> Post '[JSON] Int64

employeesAPI :: Proxy EmployeesAPI
employeesAPI = Proxy :: Proxy EmployeesAPI

fetchEmployeeH :: PGInfo -> Int64 -> Handler Employee
fetchEmployeeH  connString eid = do
  maybeEmployee <- liftIO $ (Right <$> fetchEmployeeQ connString eid) `catch` (\(e :: SomeException) ->  return (Left e) )
  case maybeEmployee of
    Left err -> Handler $ (throwE $ err400 { errBody = ("got exception :") <> (BLU.fromString $ show err) })
    Right (Just emp) -> return emp
    Right Nothing -> Handler $ (throwE $ err400 { errBody = "Could not find employee with that ID" })


fetchEmployeeByNameH :: PGInfo -> Maybe Text -> Handler [Employee]
fetchEmployeeByNameH connString empN = do
  empList <- do 
    liftIO $ do 
      employees <- (Right <$> selectByName connString empN) `catch` (\(e :: SomeException) ->  return (Left e) )
      return $ (fmap.fmap) entityVal employees
  case empList of 
      Left err -> Handler $ (throwE $ err400 { errBody = ("got exception :") <> (BLU.fromString $ show err) })
      Right [] -> Handler $ (throwE $ err400 { errBody = "Could not find employee with the Name " })
      Right suc-> return suc


createEmployeeH ::  PGInfo -> Employee -> Handler Int64
createEmployeeH connString emp = do
  res <- liftIO $ (Right <$> createEmployeeQ connString emp) `catch` (\(e :: SomeException) ->  return (Left e) )
  case res of
    Left err ->  Handler $ (throwE $ err400 { errBody = ("got exception :") <> (BLU.fromString $ show err) })
    Right suc -> return suc

server ::  PGInfo -> Server EmployeesAPI
server pGInfo = 
  (fetchEmployeeH pGInfo ) :<|> 
  (fetchEmployeeByNameH pGInfo) :<|>
  (createEmployeeH  pGInfo )

runServer :: AppConfig -> IO ()
runServer AppConfig {..} =  
  withStdoutLogger $ \aplogger -> do
        let settings = setPort appPort $ setLogger aplogger defaultSettings
        runSettings settings (serve employeesAPI (server $localConnString dbConfig ))

