module Main where

import AWSLambda ( lambdaMain )
import qualified Data.Aeson as Aeson
import System.Environment (getEnv)
import qualified Data.ByteString.Char8 as B

main :: IO ()
main = lambdaMain handler

handler :: Aeson.Value -> IO ()
handler _ = do
    username <- getEnv "DB_USERNAME"
    password <- getEnv "DB_PASSWORD"
    sqlFile <- readFile "init.sql"
    
    putStrLn "Done!"