{-# LANGUAGE  RecordWildCards #-}
module Main where

import Data.Aeson
import Options.Applicative
import Server
import Config 
import Database


main :: IO ()
main =  do
    ConfigPathArg{..}  <- execParser parserInfo
    config <- eitherDecodeFileStrict configFilePath
    case config of
        Right appConfig@AppConfig{..} -> migrateDB  (localConnString dbConfig) >> runServer appConfig
        Left err -> error $ "unable to parse  config file : " <> err


configPathParser :: Parser  ConfigPathArg
configPathParser = ConfigPathArg 
  <$> strOption 
      (  long "configFilePath"
      <> metavar "String"
      <> help "configFilePath" )

parserInfo :: ParserInfo ConfigPathArg
parserInfo = info (configPathParser <**> helper)
  ( fullDesc
  <> progDesc "WebServer"
  <> header "WebServer" )

