{-# LANGUAGE UnicodeSyntax #-}
module Main where

import           AWSLambda             (lambdaMain)
import qualified Data.Aeson            as Aeson
import qualified Data.ByteString.Char8 as B
import           Database.MySQL.Base   (ConnectInfo (connectHost, connectPassword, connectUser),
                                        connect, defaultConnectInfo, query)
import           System.Environment    (getEnv)

main ∷ IO ()
main = lambdaMain handler

handler ∷ Aeson.Value → IO ()
handler _ = do
    username <- getEnv "DB_USERNAME"
    password <- getEnv "DB_PASSWORD"
    sqlFile <- readFile "init.sql"
    putStrLn "Connecting..."
    conn <- connect defaultConnectInfo {
        connectHost = "websites-dev-visitsdb-wt73yj8godix.cluster-c3bfry1faakf.eu-west-2.rds.amazonaws.com",
        connectUser = username,
        connectPassword = password
    }
    putStrLn "Connected. Querying..."
    query conn $ B.pack sqlFile
    putStrLn "Done!"
