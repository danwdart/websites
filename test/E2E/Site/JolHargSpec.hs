{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE UnicodeSyntax     #-}

module E2E.Site.JolHargSpec where

import           E2E.Site.Util (runTest)
import           Site.JolHarg  (serve)
import           Test.Hspec    (Spec, describe, it)

spec ∷ Spec
spec = do
    describe "JolHarg" .
        it "serves and checks sizes" $
            runTest "jolharg" ["Free Software", "Contact"] serve