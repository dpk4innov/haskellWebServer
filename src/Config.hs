{-# Language DerivingStrategies #-}
{-# Language DeriveAnyClass  #-}
{-# Language DeriveGeneric #-}


module Config where

import Data.Text
import Data.Aeson
import GHC.Generics

data AppConfig = AppConfig {
       dbConfig :: DBConfig,
       appPort :: Int
    } 
    deriving (Show,Eq,Generic)
    deriving anyclass (ToJSON,FromJSON)


data DBConfig = DBConfig
    {
        dbHost :: Text,
        dbPort :: Int,
        dbUsername :: Text,
        dbPassword ::Text,
        dbName :: Text
    } 
    deriving (Show,Eq,Generic)
    deriving anyclass (ToJSON,FromJSON)

newtype  ConfigPathArg = ConfigPathArg {configFilePath :: String} deriving (Show,Eq)
