{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE UnicodeSyntax     #-}

module Html.MadHacker.Index (page, page404) where

import           Data.Env
import           Data.Site.MadHacker
import           Html.Common.Head

import           Html.Common.Bootstrap
import           Html.Common.Error.NotFound
import           Html.Common.Page
import           Text.Blaze.Html5            as H hiding (main)
import           Text.Blaze.Html5.Attributes as A

pageReviews ∷ Html → Html → WebsiteM Html
pageReviews reviewLinks reviews = makePage "reviews" "Reviews" customLayout defaultPage $ do
    row $ do
        H.div ! class_ "col-md-2 py-3 mb-3" $ reviewLinks
        H.div ! class_ "col-md-8 py-3 mb-3 bg-light" $ reviews

htmlHeader ∷ Html → Html → WebsiteM Html
htmlHeader reviewLinks reviews = do
    urlDanDart' <- asks urlDanDart
    pageReviews' <- pageReviews reviewLinks reviews
    (pure . (nav ! class_ "p-0 p-sm-2 navbar d-block d-sm-flex navbar-expand navbar-dark bg-primary")) . (H.div ! class_ "container-fluid my-0") $ (do
            a ! class_ "p-0 pt-1 pt-sm-0 w-sm-auto text-center text-sm-start navbar-brand" ! href "#reviews" $ do
                img ! src "/img/favicon.png" ! A.style "height:32px" ! alt ""
                H.span ! class_ "title ms-2" $ "The Mad Hacker: Reviews"
            H.div . (ul ! class_ "navbar-nav px-3") $ do
                    extNav (textValue urlDanDart') "Dan Dart"
                    pageReviews'
                    dlNav "/atom.xml" "Atom Feed")

extraHead ∷ Html
extraHead = link ! rel "alternate" ! type_ "application/atom+xml" ! A.title "The Mad Hacker: Reviews" ! href "/atom.xml"

page ∷ Html → Html → WebsiteM Html
page reviewLinks reviews = do
    header' <- htmlHeader reviewLinks reviews
    head' <- htmlHead descTitle keywords extraHead
    pure . (docTypeHtml ! lang "en-GB") $ do
        head'
        header'

page404 ∷ WebsiteM Html
page404 = defaultPage404 descTitle keywords extraHead
