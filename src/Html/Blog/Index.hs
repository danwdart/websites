{-# LANGUAGE OverloadedStrings #-}

module Html.Blog.Index (page) where

import Data.Blog

import Html.Common.Head
import Html.Common.Link

import Text.Blaze.Html5 as H hiding (main)
import Text.Blaze.Html5.Attributes as A

pageIntro :: Html
pageIntro = li ! class_ "nav-item" $ do
    input ! type_ "radio" ! A.style "display:none" ! checked "checked" ! name "selected" ! A.id "Intro" ! value "Intro"
    H.label ! class_ "mb-0" ! for "Intro" $ a ! class_ "nav-link btn btn-sm" $ "Intro"
    H.div ! class_ "page" ! A.id "intro" $ do
        H.div ! class_ "row" $ H.div ! class_ "col my-md-3" $ small "» Intro"
        H.div ! class_ "row" $ H.div ! class_ "col-md-8 offset-md-2 py-3 mb-3 bg-light" $
            p "Blog"

htmlHeader :: Html
htmlHeader = nav ! class_ "p-0 p-sm-2 navbar d-block d-sm-flex navbar-expand navbar-dark bg-primary" $
    a ! class_ "w-25 p-0 pt-1 pt-sm-0 w-sm-auto text-center text-sm-left navbar-brand" ! href "#intro" $ do
        img ! src "/img/favicon.png" ! A.style "height:32px" ! alt ""
        H.span ! class_ "title ml-2" $ "Dan Dart"

page :: Html -> Html
page blogPosts = docTypeHtml ! lang "en-GB" $ do
    htmlHead descTitle keywords
    htmlHeader
    body blogPosts