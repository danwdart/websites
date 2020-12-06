{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE UnicodeSyntax     #-}
module Main where

import           AWSLambda                (APIGatewayProxyRequest,
                                           APIGatewayProxyResponse,
                                           agprqHeaders,
                                           agprqQueryStringParameters,
                                           agprqRequestContext, agprsHeaders,
                                           apiGatewayMain, prcIdentity,
                                           response, responseBody, responseOK,
                                           riSourceIp)
import           Control.Monad            (unless, when)
import           Control.Monad.IO.Class   (MonadIO (liftIO))
import           Control.Monad.Trans.Cont (ContT (runContT), callCC)
import           Data.Aeson               (encode, (.=), object)
import qualified Data.ByteString.Char8    as B
import qualified Data.ByteString.Lazy.Char8    as BL
import qualified Data.ByteString.Search as SS
import           Data.Maybe               (fromMaybe)
import           Data.Text                as T (Text)
import           Data.Time                (UTCTime, defaultTimeLocale,
                                           formatTime, getCurrentTime)
import           Database.MySQL.Base      (ConnectInfo (connectDatabase, connectHost, connectPassword, connectUser),
                                           connect, defaultConnectInfo, escape,
                                           query)
import           Network.AWS.Lens         ((&), (.~), (?~), (^.))
import           System.Environment       (getEnv)
import           Text.Printf              (printf)
import Data.ByteString.Base64

main ∷ IO ()
main = apiGatewayMain handler

mysqlFormat ∷ String
mysqlFormat = "%Y-%m-%d %H:%M:%S"

formatToMySQL ∷ UTCTime → String
formatToMySQL = formatTime defaultTimeLocale "%Y-%m-%d %H:%M:%S"

unauthorisedResponse ∷ APIGatewayProxyResponse B.ByteString
unauthorisedResponse = response 401
    & agprsHeaders .~ [("WWW-Authenticate", "Basic realm=\"JolHarg Visit Viewer\", charset=\"UTF-8\"")]
    & responseBody ?~ BL.toStrict (encode (object [
        "message" .= ("Oops, you need to authenticate." :: Text)
        ]))

handler ∷ APIGatewayProxyRequest Text → IO (APIGatewayProxyResponse B.ByteString)
handler request = flip runContT return . callCC $ \kill -> do
    liftIO $ putStrLn "Getting envs..."
    [username, password, host, basicUsername, basicPassword] <- sequence $ liftIO . getEnv <$> ["DB_USERNAME", "DB_PASSWORD", "DB_HOST", "BASIC_USERNAME", "BASIC_PASSWORD"]
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

    when (basicUsername == "" || basicPassword == "") .
        kill $ unauthorisedResponse
    let basicAuth = fromMaybe "" $ request ^. agprqHeaders & lookup "Authorization"
    unless ("Basic " `B.isPrefixOf` basicAuth) .
        kill $ unauthorisedResponse
    let base64Auth = B.drop 6 basicAuth
    let auth = decodeBase64Lenient base64Auth
    let [inputUsername, inputPassword] = SS.split ":" auth
    unless (
        B.pack basicUsername == inputUsername &&
        B.pack basicPassword == inputPassword
        ) .
        kill $ unauthorisedResponse

    liftIO $ putStrLn "Responding..."
    pure $ responseOK
      & agprsHeaders .~ [("Content-Type", "application/json")]
      & responseBody ?~ BL.toStrict (encode (object [
          "status" .= ("OK" :: Text)
      ]))
      -- & agprsHeaders .~ [("Content-Type", "image/gif")]
      -- & responseBody ?~ decodeBase64Lenient "R0lGODlhAQABAAAAACH5BAEKAAEALAAAAAABAAEAAAICTAEAOw=="
