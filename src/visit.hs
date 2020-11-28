{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE UnicodeSyntax     #-}
module Main where

import           AWSLambda             (APIGatewayProxyRequest,
                                        APIGatewayProxyResponse, agprqHeaders,
                                        agprqQueryStringParameters,
                                        agprqRequestContext, agprsHeaders,
                                        apiGatewayMain, prcIdentity,
                                        responseBody, responseOK, riSourceIp)
import           Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Char8 as B
import           Data.Maybe            (fromMaybe)
import           Data.Text             as T (Text)
import           Data.Time             (UTCTime, defaultTimeLocale, formatTime,
                                        getCurrentTime)
import           Database.MySQL.Base   (ConnectInfo (connectDatabase, connectHost, connectPassword, connectUser),
                                        connect, defaultConnectInfo, escape,
                                        query)
import           Network.AWS.Lens      ((&), (.~), (?~), (^.))
import           System.Environment    (getEnv)
import           Text.Printf           (printf)

main ∷ IO ()
main = apiGatewayMain handler

mysqlFormat ∷ String
mysqlFormat = "%Y-%m-%d %H:%M:%S"

formatToMySQL ∷ UTCTime → String
formatToMySQL = formatTime defaultTimeLocale "%Y-%m-%d %H:%M:%S"

handler ∷ APIGatewayProxyRequest Text → IO (APIGatewayProxyResponse ByteString)
handler request = do
    username <- getEnv "DB_USERNAME"
    password <- getEnv "DB_PASSWORD"
    host <- getEnv "DB_HOST"
    putStrLn $ username <> " " <> password <> " " <> host
    time <- formatToMySQL <$> getCurrentTime
    conn <- connect defaultConnectInfo {
        connectHost = host,
        connectUser = username,
        connectPassword = password,
        connectDatabase = "visits"
    }
    let ip = maybe "" show $ request ^. agprqRequestContext . prcIdentity . riSourceIp
    sua <- escape conn . fromMaybe "" $ request ^. agprqHeaders & lookup "User-Agent"
    surl <- escape conn . maybe "" (fromMaybe "") $ request ^. agprqQueryStringParameters & lookup "url"
    let ua = B.unpack sua
    let url = B.unpack surl
    putStrLn $ printf "INSERT INTO `visits`.`visits` (url, ua, ip, time) values (\"%s\", \"%s\", \"%s\", \"%s\")" url ua ip time
    query conn . B.pack $ printf "INSERT INTO `visits`.`visits` (url, ua, ip, time) values (\"%s\", \"%s\", \"%s\", \"%s\")" url ua ip time
    pure $ responseOK
      & agprsHeaders .~ [("Content-Type", "text/plain")]
      & responseBody ?~ "Hi!"
      -- & agprsHeaders .~ [("Content-Type", "image/gif")]
      -- & responseBody ?~ decodeBase64Lenient "R0lGODlhAQABAAAAACH5BAEKAAEALAAAAAABAAEAAAICTAEAOw=="
