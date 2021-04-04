{-# LANGUAGE BlockArguments    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE UnicodeSyntax     #-}
module Main where

import           AWSLambda.Events.APIGateway
import           Data.Text                   as T
import           Network.AWS.Lens

main ∷ IO ()
main = apiGatewayMain handler

handler ∷ APIGatewayProxyRequest Text → IO (APIGatewayProxyResponse Text)
handler _ = do
    pure $ responseOK & agprsHeaders .~ [("Content-Type", "text/plain")] & responseBody ?~ "Hi!"
