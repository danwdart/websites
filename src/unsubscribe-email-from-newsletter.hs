{-# LANGUAGE BlockArguments    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE UnicodeSyntax     #-}
module Main where
    
import           AWSLambda
import           AWSLambda.Events.APIGateway
import           Control.Monad
import           Data.Aeson                  hiding (object)
import           Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Char8 as B
import           Data.ByteString.Lazy.Base64
import qualified Data.ByteString.Lazy.Char8  as BSL
import           Data.Char
import           Data.Maybe
import           Data.Text             as T
import           Data.Text.Encoding
import           Data.Time
import           Data.Time.Format.ISO8601
import           Database.MySQL.Base
import           GHC.Generics
import           Network.AWS.Data.Query
import           Network.AWS.Lens
import           Network.HTTP.Types
import           System.Environment
import           Text.Printf
import           Util.QueryString

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
