{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE UnicodeSyntax     #-}
module Main where

import AWSLambda
    ( agprqHeaders,
      agprqQueryStringParameters,
      agprqRequestContext,
      agprsHeaders,
      apiGatewayMain,
      prcIdentity,
      responseBody,
      responseOK,
      riSourceIp,
      APIGatewayProxyRequest,
      APIGatewayProxyResponse )
import           Data.ByteString.Char8       (ByteString)
import qualified Data.ByteString.Char8       as B
import           Data.Text                   as T (Text)
import           Data.Time                   (defaultTimeLocale, formatTime, UTCTime, getCurrentTime)
import           Database.MySQL.Base         (escape, ConnectInfo (connectDatabase, connectHost, connectPassword, connectUser),
                                              connect, defaultConnectInfo,
                                              query)
import           Network.AWS.Lens            ((&), (.~), (?~), (^.))
import           System.Environment          (getEnv)
import           Text.Printf                 (printf)
import Data.Maybe (fromMaybe)

main ∷ IO ()
main = apiGatewayMain handler

mysqlFormat :: String
mysqlFormat = "%Y-%m-%d %H:%M:%S"

formatToMySQL :: UTCTime -> String
formatToMySQL = formatTime defaultTimeLocale "%Y-%m-%d %H:%M:%S"

handler ∷ APIGatewayProxyRequest Text → IO (APIGatewayProxyResponse ByteString)
handler request = do
    username <- getEnv "DB_USERNAME"
    password <- getEnv "DB_PASSWORD"
    time <- formatToMySQL <$> getCurrentTime
    conn <- connect defaultConnectInfo {
        connectHost = "websites-dev-visitsdb-wt73yj8godix.cluster-c3bfry1faakf.eu-west-2.rds.amazonaws.com",
        connectUser = username,
        connectPassword = password,
        connectDatabase = "visits"
    }
    let ip = maybe "" show $ request ^. agprqRequestContext . prcIdentity . riSourceIp
    sua <- escape conn . fromMaybe "" $ request ^. agprqHeaders & lookup "User-Agent"
    surl <- escape conn . maybe "" (fromMaybe "") $ request ^. agprqQueryStringParameters & lookup "url"
    let ua = B.unpack sua
    let url = B.unpack surl
    query conn . B.pack $ printf "INSERT INTO `visits`.`visits` (url, ua, ip, time) values (\"%s\", \"%s\", \"%s\", \"%s\")" url ua ip time
    pure $ responseOK
      & agprsHeaders .~ [("Content-Type", "text/plain")]
      & responseBody ?~ "Hi!"
      -- & agprsHeaders .~ [("Content-Type", "image/gif")]
      -- & responseBody ?~ decodeBase64Lenient "R0lGODlhAQABAAAAACH5BAEKAAEALAAAAAABAAEAAAICTAEAOw=="
