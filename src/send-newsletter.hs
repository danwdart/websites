{-# LANGUAGE UnicodeSyntax #-}
module Main where

import           AWSLambda    (lambdaMain)
import qualified Data.Aeson   as Aeson
import           Data.Text.IO

main ∷ IO ()
main = lambdaMain handler

handler ∷ Aeson.Value → IO ()
handler _ = do
    putStrLn "Done!"
