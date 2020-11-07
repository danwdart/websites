{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE UnicodeSyntax     #-}

module E2E.Site.M0ORISpec where

import           E2E.Site.Util (runTest)
import           Site.M0ORI    (serve)
import           Test.Hspec    (Spec, describe, it)

spec ∷ Spec
spec = do
    describe "M0ORI" .
        it "serves and checks sizes" $
            runTest "m0ori" serve