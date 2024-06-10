module Main where

import Data.Aeson
import Server


main :: IO ()
main =  do
    config <- eitherDecodeFileStrict "/home/dj/webServer/app.json"
    case config of
        Right appConfig -> runServer appConfig
        Left err -> error err

