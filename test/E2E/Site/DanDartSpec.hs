{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE UnicodeSyntax     #-}

module E2E.Site.DanDartSpec where

import           E2E.Site.Util (runTest)
import           Site.DanDart  (serve)
import           Test.Hspec    (Spec, describe, it)

spec ∷ Spec
spec = do
    describe "DanDart" .
        it "serves and checks sizes" $
            runTest "dandart" serve