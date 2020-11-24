{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE UnicodeSyntax     #-}
module Main where

import           AWSLambda.Events.APIGateway
-- import Data.ByteString.Base64
import           Data.Text                   as T
import Data.Time
import           Network.AWS.Lens
import Data.ByteString.Char8 (ByteString)
-- import System.Environment (getEnv)

main ∷ IO ()
main = apiGatewayMain handler

handler ∷ APIGatewayProxyRequest Text → IO (APIGatewayProxyResponse ByteString)
handler request = do
    -- login <- getEnv "DB_LOGIN"
    -- password <- getEnv "DB_PASSWORD"
    time <- getCurrentTime
    print time
    print $ request ^. agprqHeaders & lookup "User-Agent"
    print $ request ^. agprqHeaders & lookup "Host"
    pure $ responseOK
      & agprsHeaders .~ [("Content-Type", "text/plain")]
      & responseBody ?~ "Hi!"
      -- & agprsHeaders .~ [("Content-Type", "image/gif")]
      -- & responseBody ?~ decodeBase64Lenient "R0lGODlhAQABAAAAACH5BAEKAAEALAAAAAABAAEAAAICTAEAOw=="