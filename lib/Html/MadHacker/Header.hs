{-# LANGUAGE OverloadedStrings #-}

module Html.MadHacker.Header where

import Control.Lens
import Control.Monad.Reader
import Data.Env.Types
import Html.Common.Page
import Html.MadHacker.Page.Reviews
import Text.Blaze.Html5            as H hiding (main)
import Text.Blaze.Html5.Attributes as A

htmlHeader ∷ MonadReader Website m ⇒ Html → Html → Html → m Html
htmlHeader reviewLinks reviewTagLinks reviews' = do
    urlDanDart' <- view $ urls . urlDanDart
    pageReviews' <- pageReviews reviewLinks reviewTagLinks reviews'
    atomXml' <- view $ siteType . atomUrl . to show
    (pure . (nav ! class_ "p-0 p-sm-2 navbar d-block d-sm-flex navbar-expand navbar-dark bg-primary")) .
        (H.div ! class_ "row my-0 w-100") $ (do
            a ! class_ "p-0 pt-1 pt-sm-0 col w-sm-auto text-center text-sm-start navbar-brand" ! href "#reviews" $ do
                img ! src "/img/favicon.png" ! A.style "height:32px" ! alt ""
                H.span ! class_ "title ms-2" $ "The Mad Hacker: Reviews"
            (H.div ! class_ "col") . (ul ! class_ "navbar-nav px-3") $ do
                    extNav (stringValue (show urlDanDart')) "Dan Dart"
                    pageReviews'
                    dlNav (toValue $ atomXml') "Atom Feed"
        )
