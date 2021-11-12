{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE UnicodeSyntax     #-}

module Html.MadHacker.Header where

import           Control.Monad.Trans.Reader
import           Data.Env
import           Data.Site.MadHacker
import           Html.Common.Bootstrap
import           Html.Common.Error.NotFound
import           Html.Common.Head
import           Html.Common.Page
import           Html.MadHacker.Page.Reviews
import           Text.Blaze.Html5            as H hiding (main)
import           Text.Blaze.Html5.Attributes as A

htmlHeader ∷ Html → Html → WebsiteM Html
htmlHeader reviewLinks reviews = do
    urlDanDart' <- asks (urlDanDart . urls)
    pageReviews' <- pageReviews reviewLinks reviews
    (pure . (nav ! class_ "p-0 p-sm-2 navbar d-block d-sm-flex navbar-expand navbar-dark bg-primary")) .
        (H.div ! class_ "row my-0 w-100") $ (do
            a ! class_ "p-0 pt-1 pt-sm-0 col w-sm-auto text-center text-sm-start navbar-brand" ! href "#reviews" $ do
                img ! src "/img/favicon.png" ! A.style "height:32px" ! alt ""
                H.span ! class_ "title ms-2" $ "The Mad Hacker: Reviews"
            (H.div ! class_ "col") . (ul ! class_ "navbar-nav px-3") $ do
                    extNav (textValue urlDanDart') "Dan Dart"
                    pageReviews'
                    dlNav "/atom.xml" "Atom Feed")
