module Main where

import AWSLambda ( lambdaMain )
import qualified Data.Aeson as Aeson
import Database.MySQL.Base
    ( connect,
      defaultConnectInfo,
      query,
      ConnectInfo(connectHost, connectUser, connectPassword,
                  connectDatabase) )
import System.Environment (getEnv)
import qualified Data.ByteString.Char8 as B

main :: IO ()
main = lambdaMain handler

handler :: Aeson.Value -> IO ()
handler _ = do
    username <- getEnv "DB_USERNAME"
    password <- getEnv "DB_PASSWORD"
    sqlFile <- readFile "init.sql"
    conn <- connect defaultConnectInfo {
        connectHost = "visits.cluster-c3bfry1faakf.eu-west-2.rds.amazonaws.com",
        connectUser = username,
        connectPassword = password,
        connectDatabase = "visits"
    }
    query conn $ B.pack sqlFile
    putStrLn "Done!"