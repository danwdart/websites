{-# LANGUAGE BlockArguments    #-}
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

handler ∷ APIGatewayProxyRequest Text → IO (APIGatewayProxyResponse Text)
handler request = do
    let qs = parseQueryString . encodeUtf8 $ fromMaybe "" (request ^. requestBody)
    let lookupQS = decodeUtf8 . urlDecode True . lookupQueryString qs
    let memail = lookupQS "email"
    
    if isJust memail then do
        [username, password, host] <- sequence $ getEnv <$> ["DB_USERNAME", "DB_PASSWORD", "DB_HOST"]
        conn <- connect defaultConnectInfo {
            connectHost = host,
            connectUser = username,
            connectPassword = password,
            connectDatabase = "newsletters"
        }

        email <- escape conn . maybe "" email
        putStrLn "Querying..."
        query conn $ "UPDATE `newsletters`.`visits` SET active = FALSE WHERE email = \"" <> email <> "\""    

        pure $ responseOK & agprsHeaders .~ [("Content-Type", "text/html")] & responseBody ?~ "<span style=\"color:green\">You have been unsubscribed. Sorry to see you go!</span>"
    else do
        pure $ responseOK & agprsHeaders .~ [("Content-Type", "text/html")] & responseBody ?~ "<span style=\"color:orange\">You have entered an invalid email address Want to try again?.</span>"
