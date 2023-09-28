module Main where

import Build
-- https://hackage.haskell.org/package/criterion
-- http://www.serpentine.com/criterion/tutorial.html
import Criterion.Main

main ∷ IO ()
main = defaultMain [
    bgroup "build" [
        bench "build" $ nfIO runBuild
    ]
    ]
