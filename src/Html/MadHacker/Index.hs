{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE UnicodeSyntax     #-}

module Html.MadHacker.Index (page, page404) where

import           Data.MadHacker

import           Html.Common.Head

import           Html.Common.Bootstrap
import           Html.Common.Error.NotFound
import           Html.Common.Page
import           Html.Common.Visit
import           Text.Blaze.Html5            as H hiding (main)
import           Text.Blaze.Html5.Attributes as A

pageReviews ∷ Html → Html → Html
pageReviews reviewLinks reviews = makePage "reviews" "Reviews" customLayout defaultPage $ do
    row $ do
        H.div ! class_ "col-md-2 py-3 mb-3" $ reviewLinks
        H.div ! class_ "col-md-8 py-3 mb-3 bg-light" $ reviews

htmlHeader ∷ Html → Html → Html
htmlHeader reviewLinks reviews = nav ! class_ "p-0 p-sm-2 navbar d-block d-sm-flex navbar-expand navbar-dark bg-primary" $ do
    a ! class_ "p-0 pt-1 pt-sm-0 w-sm-auto text-center text-sm-left navbar-brand" ! href "#reviews" $ do
        img ! src "/img/favicon.png" ! A.style "height:32px" ! alt ""
        H.span ! class_ "title ml-2" $ "The Mad Hacker: Reviews"
    H.div . (ul ! class_ "navbar-nav px-3") $ do
            extNav  "https://dandart.co.uk" "Dan Dart"
            pageReviews reviewLinks reviews
            dlNav "/atom.xml" "Atom Feed"

extraHead ∷ Html
extraHead = link ! rel "alternate" ! type_ "application/atom+xml" ! A.title "The Mad Hacker: Reviews" ! href "/atom.xml"

page ∷ Bool -> Html → Html → Html
page dev reviewLinks reviews = docTypeHtml ! lang "en-GB" $ do
    htmlHead dev descTitle keywords extraHead
    htmlHeader reviewLinks reviews
    visit "madhacker"

page404 ∷ Html
page404 = defaultPage404 descTitle keywords $ do
    extraHead
    visit "madhacker404"
