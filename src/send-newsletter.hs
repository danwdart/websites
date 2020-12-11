{-# LANGUAGE UnicodeSyntax #-}
module Main where

import           AWSLambda             (lambdaMain)
import qualified Data.Aeson            as Aeson
import qualified Data.ByteString.Char8 as B
import           Database.MySQL.Base   (ConnectInfo (connectDatabase, connectHost, connectPassword, connectUser),
                                        connect, defaultConnectInfo, query)
import           System.Environment    (getEnv)

main ∷ IO ()
main = lambdaMain handler

handler ∷ Aeson.Value → IO ()
handler _ = do
    putStrLn "Done!"
