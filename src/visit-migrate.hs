{-# LANGUAGE UnicodeSyntax #-}
module Main where

import           AWSLambda             (lambdaMain)
import qualified Data.Aeson            as Aeson
import qualified Data.ByteString.Char8 as B
import           Database.MySQL.Base   (ConnectInfo (connectDatabase, connectHost, connectPassword, connectUser),
                                        connect, defaultConnectInfo, query)
import           System.Environment    (getEnv)

main ∷ IO ()
main = lambdaMain handler

handler ∷ Aeson.Value → IO ()
handler _ = do
    username <- getEnv "DB_USERNAME"
    password <- getEnv "DB_PASSWORD"
    host <- getEnv "DB_HOST"
    sqlFile <- B.readFile "init.sql"
    putStrLn "Connecting..."
    conn <- connect defaultConnectInfo {
        connectHost = host,
        connectUser = username,
        connectPassword = password,
        connectDatabase = "mysql"
    }
    putStrLn "Connected. Querying..."
    mapM_ (query conn) . filter (not . B.null) . fmap (B.dropWhile (=='\n')) . B.split ';' $ sqlFile
    putStrLn "Done!"
