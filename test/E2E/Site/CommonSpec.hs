{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE UnicodeSyntax     #-}

module E2E.Site.CommonSpec where

import           E2E.Site.Util (runTest)
import qualified Site.Blog     as B
import qualified Site.DanDart  as D
import qualified Site.JolHarg  as J
import qualified Site.M0ORI    as M
import           Test.Hspec    (runIO, Spec, describe, it, shouldBe)
import Data.List (nub)

spec ∷ Spec
spec = mapM_ (\(siteName, serve) ->
    describe siteName $ do
        sizes <- runIO $ runTest siteName serve
        mapM_ (\(configName, sizeMap) ->
            describe configName $
                mapM_ (\((winWidth, winHeight), (_, navHeight), cardSizes) -> do
                    describe (show winWidth <> "x" <> show winHeight) . it "nav height is equal to 39" $ (
                            navHeight `shouldBe` 39)
                    it "cards are only one size" $
                        ((length . nub $ cardSizes )`shouldBe` 1)
                ) sizeMap
            ) sizes
    ) [
        ("blog", B.serve),
        ("dandart", D.serve),
        ("jolharg", J.serve)--,
        ("m0ori", M.serve)
    ]
