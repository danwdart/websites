{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE UnicodeSyntax     #-}
module Main where

import           AWSLambda                  (APIGatewayProxyRequest,
                                             APIGatewayProxyResponse,
                                             agprqHeaders,
                                             agprqQueryStringParameters,
                                             agprqRequestContext, agprsHeaders,
                                             apiGatewayMain, prcIdentity,
                                             responseBody, responseOK,
                                             riSourceIp)
import           Control.Monad.IO.Class     (MonadIO (liftIO))
import           Data.Aeson                 (encode, object, (.=))
import qualified Data.ByteString.Char8      as B
import qualified Data.ByteString.Lazy.Char8 as BL
import           Data.Maybe                 (fromMaybe)
import           Data.Text                  as T (Text)
import           Data.Text.IO
import           Data.Time                  (UTCTime, defaultTimeLocale,
                                             formatTime, getCurrentTime)
import           Database.MySQL.Base        (ConnectInfo (connectDatabase, connectHost, connectPassword, connectUser),
                                             connect, defaultConnectInfo,
                                             escape, query)
import           Network.AWS.Lens           ((&), (.~), (?~), (^.))
import           System.Environment         (getEnv)
import           Text.Printf                (printf)

main ∷ IO ()
main = apiGatewayMain handler

mysqlFormat ∷ String
mysqlFormat = "%Y-%m-%d %H:%M:%S"

formatToMySQL ∷ UTCTime → String
formatToMySQL = formatTime defaultTimeLocale "%Y-%m-%d %H:%M:%S"

handler ∷ APIGatewayProxyRequest Text → IO (APIGatewayProxyResponse B.ByteString)
handler request = do
    liftIO $ putStrLn "Getting envs..."
    [username, password, host] <- sequence $ liftIO . getEnv <$> ["DB_USERNAME", "DB_PASSWORD", "DB_HOST"]
    time <- formatToMySQL <$> liftIO getCurrentTime
    liftIO $ putStrLn "Connecting..."
    conn <- liftIO $ connect defaultConnectInfo {
        connectHost = host,
        connectUser = username,
        connectPassword = password,
        connectDatabase = "visits"
    }
    let ip = maybe "" show $ request ^. agprqRequestContext . prcIdentity . riSourceIp
    liftIO $ putStrLn "Escaping..."
    sua <- liftIO . escape conn . fromMaybe "" $ request ^. agprqHeaders & lookup "User-Agent"
    surl <- liftIO . escape conn . maybe "" (fromMaybe "") $ request ^. agprqQueryStringParameters & lookup "url"
    let ua = B.unpack sua
    let url = B.unpack surl
    liftIO $ putStrLn "Logging..."
    liftIO . query conn . B.pack $ printf "INSERT INTO `visits`.`adminvisits` (url, ua, ip, time) values (\"%s\", \"%s\", \"%s\", \"%s\")" url ua ip time
    {-
    liftIO $ putStrLn "Querying..."
    liftIO . query conn . B.pack $ "SELECT * FROM `visits`.`visits`"
    result <- liftIO . storeResult $ conn
    -}

    liftIO $ putStrLn "Responding..."
    pure $ responseOK
      & agprsHeaders .~ [("Content-Type", "application/json")]
      & responseBody ?~ BL.toStrict (encode (object [
          "status" .= ("OK" :: Text)
      ]))
