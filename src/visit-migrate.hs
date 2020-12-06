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
    [username, password, host] <- sequence $ getEnv <$> ["DB_USERNAME", "DB_PASSWORD", "DB_HOST"]
    sqlFile <- B.readFile "sql/init.sql"
    putStrLn "Connecting..."
    conn <- connect defaultConnectInfo {
        connectHost = host,
        connectUser = username,
        connectPassword = password,
        connectDatabase = "mysql"
    }
    putStrLn "Connected. Querying..."
    mapM_ (\q -> putStrLn ("Querying " <> B.unpack q) >> query conn q) . filter (not . B.null) . fmap (B.dropWhile (=='\n')) . B.split ';' $ sqlFile
    putStrLn "Done!"
