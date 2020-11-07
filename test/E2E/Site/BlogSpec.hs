{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE UnicodeSyntax     #-}

module E2E.Site.BlogSpec where

import           E2E.Site.Util (runTest)
import           Site.Blog     (serve)
import           Test.Hspec    (Spec, describe, it)

spec ∷ Spec
spec = do
    describe "Blog" .
        it "serves and checks sizes" $
            runTest "blog" serve
