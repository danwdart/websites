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
import           Data.Time                   (getCurrentTime)
import           Database.MySQL.Base         (ConnectInfo (connectDatabase, connectHost, connectPassword, connectUser),
                                              connect, defaultConnectInfo,
                                              query)
import           Network.AWS.Lens            ((&), (.~), (?~), (^.))
import           System.Environment          (getEnv)
import           Text.Printf                 (printf)

main ∷ IO ()
main = apiGatewayMain handler

handler ∷ APIGatewayProxyRequest Text → IO (APIGatewayProxyResponse ByteString)
handler request = do
    username <- getEnv "DB_USERNAME"
    password <- getEnv "DB_PASSWORD"
    time <- show <$> getCurrentTime
    -- print time
    print request
    let ua = maybe "" B.unpack $ request ^. agprqHeaders & lookup "User-Agent"
    let ip = maybe "" show $ request ^. agprqRequestContext . prcIdentity . riSourceIp
    -- let url = maybe "" B.unpack $ request ^. agprqHeaders & lookup "Referrer"
    let url = maybe "" (maybe "" B.unpack) (request ^. agprqQueryStringParameters & lookup "url")
    print ua
    print ip
    print url
    conn <- connect defaultConnectInfo {
        connectHost = "websites-dev-visitsdb-wt73yj8godix.cluster-c3bfry1faakf.eu-west-2.rds.amazonaws.com",
        connectUser = username,
        connectPassword = password,
        connectDatabase = "visits"
    }
    query conn . B.pack $ printf "INSERT INTO `visits`.`visits` (url, ua, ip, time) values (\"%s\", \"%s\", \"%s\", \"%s\")" url ua ip time
    pure $ responseOK
      & agprsHeaders .~ [("Content-Type", "text/plain")]
      & responseBody ?~ "Hi!"
      -- & agprsHeaders .~ [("Content-Type", "image/gif")]
      -- & responseBody ?~ decodeBase64Lenient "R0lGODlhAQABAAAAACH5BAEKAAEALAAAAAABAAEAAAICTAEAOw=="
