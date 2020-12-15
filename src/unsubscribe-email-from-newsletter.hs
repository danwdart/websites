{-# LANGUAGE BlockArguments    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE UnicodeSyntax     #-}
module Main where
    
import           AWSLambda
import           Data.Maybe
import           Data.Text             as T
import           Data.Text.Encoding
import           Database.MySQL.Base
import           Network.AWS.Data.Query
import           Network.AWS.Lens
import           Network.HTTP.Types
import           System.Environment
import           Util.QueryString

main ∷ IO ()
main = apiGatewayMain handler

handler ∷ APIGatewayProxyRequest Text → IO (APIGatewayProxyResponse Text)
handler request = do
    let qs = parseQueryString . encodeUtf8 $ fromMaybe "" (request ^. requestBody)
    let lookupQS = decodeUtf8 . urlDecode True . lookupQueryString qs
    let temail = lookupQS "email"
    
    if not . T.null $ temail then do
        [username, password, host] <- sequence $ getEnv <$> ["DB_USERNAME", "DB_PASSWORD", "DB_HOST"]
        conn <- connect defaultConnectInfo {
            connectHost = host,
            connectUser = username,
            connectPassword = password,
            connectDatabase = "newsletters"
        }

        email <- escape conn $ encodeUtf8 temail
        putStrLn "Querying..."
        query conn $ "UPDATE `newsletters`.`emails` SET active = FALSE WHERE email = \"" <> email <> "\""    

        pure $ responseOK & agprsHeaders .~ [("Content-Type", "text/html")] & responseBody ?~ "<span style=\"color:green\">You have been unsubscribed. Sorry to see you go!</span>"
    else do
        pure $ responseOK & agprsHeaders .~ [("Content-Type", "text/html")] & responseBody ?~ "<span style=\"color:orange\">You have entered an invalid email address Want to try again?.</span>"