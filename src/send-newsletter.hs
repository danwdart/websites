{-# LANGUAGE UnicodeSyntax #-}
module Main where

import           AWSLambda             (lambdaMain)
import qualified Data.Aeson            as Aeson

main ∷ IO ()
main = lambdaMain handler

handler ∷ Aeson.Value → IO ()
handler _ = do
    putStrLn "Done!"
