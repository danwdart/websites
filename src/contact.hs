{-# LANGUAGE BlockArguments    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE UnicodeSyntax     #-}
module Main where

import           AWSLambda.Events.APIGateway
import           Control.Monad
import qualified Data.ByteString.Char8       as B
import           Data.ByteString.Lazy.Base64
import qualified Data.ByteString.Lazy.Char8  as BSL
import           Data.Char
import           Data.Maybe
import           Data.Text                   as T
import           Data.Text.Encoding
import           GHC.Generics
import           Network.AWS.Data.Query
import           Network.AWS.Lens
import           Network.HTTP.Types
import           Network.Mail.Mime
import           Network.Mail.SMTP
import           System.Environment
import           Text.Printf
import           Util.QueryString

main ∷ IO ()
main = apiGatewayMain handler

handler ∷ APIGatewayProxyRequest Text → IO (APIGatewayProxyResponse Text)
handler request = do
    smtpServer <- getEnv "SMTP_SERVER"
    smtpPort <- getEnv "SMTP_PORT"
    smtpUseTLS <- getEnv "SMTP_TLS"
    smtpUsername <- getEnv "SMTP_USERNAME"
    smtpPassword <- getEnv "SMTP_PASSWORD"

    stoEmail <- maybe "" (fromMaybe "") $ request ^. agprqQueryStringParameters & lookup "email"
    let toEmail = B.unpack stoEmail

    let qs = parseQueryString . encodeUtf8 $ fromMaybe "" (request ^. requestBody)
    let lookupQS = decodeUtf8 . urlDecode True . lookupQueryString qs

    let name = lookupQS "name"
    let email = lookupQS "email"
    let subject = lookupQS "subject"
    let message = lookupQS "message"

    sendMailWithLoginTLS' smtpServer smtpPort smtpUsername smtpPassword $
      simpleMail email [toEmail] [] [] subject [plainTextPart message]


    pure $ responseOK
      & agprsHeaders .~ [("Content-Type", "text/html")]
      & responseBody ?~ "<span style=\"color:green\">Thank you. We will attempt to get back to you within the next working day.</span>"
