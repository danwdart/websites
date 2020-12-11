{-# LANGUAGE BlockArguments    #-}
{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE UnicodeSyntax     #-}
module Main where

import           AWSLambda.Events.APIGateway
import           Control.Monad
import           Data.Aeson                  hiding (object)
import           Data.ByteString.Char8       (ByteString)
import qualified Data.ByteString.Char8       as B
import           Data.ByteString.Lazy.Base64
import qualified Data.ByteString.Lazy.Char8  as BSL
import           Data.Char
import           Data.Maybe
import           Data.Text                   as T
import           Data.Text.Encoding
import           Data.Time
import           Data.Time.Format.ISO8601
import           GHC.Generics
import           GitHub.REST                 as GH hiding ((.:))
import           Network.AWS.Data.Query
import           Network.AWS.Lens
import           Network.HTTP.Types
import           System.Environment
import           Text.Printf

main ∷ IO ()
main = apiGatewayMain handler

handler ∷ APIGatewayProxyRequest Text → IO (APIGatewayProxyResponse Text)
handler request = do
    -- print $ request ^. agprqHeaders
    --print $ request ^. requestBody
    pure $ responseOK & agprsHeaders .~ [("Content-Type", "text/html")] & responseBody ?~ "<span style=\"color:green\">You have been added.</span>"