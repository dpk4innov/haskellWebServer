{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE UndecidableInstances       #-}
{-# LANGUAGE DataKinds #-}

module Schema where

import           Control.Monad.IO.Class ()
import           Data.Aeson
import           Data.Aeson.Types
import qualified Database.Persist.TH as PTH
import           Data.Text (Text)

PTH.share [PTH.mkPersist PTH.sqlSettings, PTH.mkMigrate "migrateAll"] [PTH.persistLowerCase|
  Employee sql=Employees
    name Text
    email Text
    age Int
    department Text
    UniqueEmail email
    deriving Show Read
|]

instance ToJSON Employee where
  toJSON emp = object 
    [ "name" .= employeeName emp
    , "email" .= employeeEmail emp
    , "age" .= employeeAge emp
    , "department" .= employeeDepartment emp
    ]

instance FromJSON Employee where
  parseJSON = withObject "Employee" parseEmployee

parseEmployee :: Object -> Parser Employee
parseEmployee o = do
  eName <- o .: "name"
  eEmail <- o .: "email"
  eAge <- o .: "age"
  eDep <- o .: "department"
  return Employee
    { employeeName = eName
    , employeeEmail = eEmail
    , employeeAge = eAge
    , employeeDepartment = eDep
    }