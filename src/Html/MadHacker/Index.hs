{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE UnicodeSyntax     #-}

module Html.MadHacker.Index (page) where

import           Data.MadHacker

import           Html.Common.Head

import           Text.Blaze.Html5            as H hiding (main)
import           Text.Blaze.Html5.Attributes as A

pageReviews ∷ Html → Html → Html
pageReviews reviewLinks reviews = li ! class_ "nav-item" $ do
    input ! type_ "radio" ! A.style "display:none" ! checked "checked" ! name "selected" ! A.id "Reviews" ! value "Reviews"
    (H.label ! class_ "mb-0" ! for "Reviews") . (a ! class_ "nav-link btn btn-sm") $ "Reviews"
    H.div ! class_ "page" ! A.id "reviews" $ do
        (H.div ! class_ "row") . (H.div ! class_ "col my-md-3") $ small "» Reviews"
        H.div ! class_ "row" $ do
            H.div ! class_ "col-md-2 py-3 mb-3" $ reviewLinks
            H.div ! class_ "col-md-8 py-3 mb-3 bg-light" $ reviews

htmlHeader ∷ Html → Html → Html
htmlHeader reviewLinks reviews = nav ! class_ "p-0 p-sm-2 navbar d-block d-sm-flex navbar-expand navbar-dark bg-primary" $ do
    a ! class_ "p-0 pt-1 pt-sm-0 w-sm-auto text-center text-sm-left navbar-brand" ! href "#reviews" $ do
        img ! src "/img/favicon.png" ! A.style "height:32px" ! alt ""
        H.span ! class_ "title ml-2" $ "The Mad Hacker: Reviews"
    H.div . (ul ! class_ "navbar-nav px-3") $
            pageReviews reviewLinks reviews

page ∷ Html → Html → Html
page reviewLinks reviews = docTypeHtml ! lang "en-GB" $ do
    htmlHead descTitle keywords $ do
        link ! rel "alternate" ! type_ "application/atom+xml" ! A.title "The Mad Hacker: Reviews" ! href "/atom.xml"
    htmlHeader reviewLinks reviews
