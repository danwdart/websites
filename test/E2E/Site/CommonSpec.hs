{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE UnicodeSyntax     #-}

module E2E.Site.CommonSpec where

import           E2E.Site.Util (runTest)
import qualified Site.Blog     as B
import qualified Site.DanDart  as D
import qualified Site.JolHarg  as J
import qualified Site.M0ORI    as M
import           Test.Hspec    (shouldSatisfy, runIO, Spec, describe, it, shouldBe)
import           Data.List     (nub)
import           Data.Text     (unpack)

spec ∷ Spec
spec = mapM_ (\(siteName, serve) ->
    describe siteName $ do
        sizesAll <- runIO $ runTest siteName serve
        mapM_ (\(configName, sizeMap) ->
            describe configName $
                mapM_ (\((winWidth, winHeight), (_, navHeight), cardSizes) ->
                    describe (show winWidth <> "x" <> show winHeight) $ do
                        it "nav height is equal to 39" $
                            navHeight `shouldBe` 39
                        mapM_ (\(linkName, sizes) ->
                            describe (unpack linkName) . it "visible cards are only one size" $ (
                                (length . nub . filter (/= (0, 0)) $ sizes )`shouldSatisfy` (< 2))
                            ) cardSizes
                    ) sizeMap
            ) sizesAll
    ) [
        ("blog", B.serve),
        ("dandart", D.serve),
        ("jolharg", J.serve),
        ("m0ori", M.serve)
    ]