{-# LANGUAGE OverloadedStrings #-}

module Html.Blog.Index (page) where

import           Data.Blog

import           Html.Common.Head
import           Html.Common.Link

import           Text.Blaze.Html5            as H hiding (main)
import           Text.Blaze.Html5.Attributes as A

pageBlog :: Html -> Html -> Html
pageBlog blogPostLinks blogPosts = li ! class_ "nav-item" $ do
    input ! type_ "radio" ! A.style "display:none" ! checked "checked" ! name "selected" ! A.id "Blog" ! value "Blog"
    H.label ! class_ "mb-0" ! for "Blog" $ a ! class_ "nav-link btn btn-sm" $ "Blog"
    H.div ! class_ "page" ! A.id "blog" $ do
        H.div ! class_ "row" $ H.div ! class_ "col my-md-3" $ small "» Blog"
        H.div ! class_ "row" $ do
            H.div ! class_ "col-md-2 py-3 mb-3" $ blogPostLinks
            H.div ! class_ "col-md-8 py-3 mb-3 bg-light" $ blogPosts

htmlHeader :: Html -> Html -> Html
htmlHeader blogPostLinks blogPosts = nav ! class_ "p-0 p-sm-2 navbar d-block d-sm-flex navbar-expand navbar-dark bg-primary" $ do
    a ! class_ "w-25 p-0 pt-1 pt-sm-0 w-sm-auto text-center text-sm-left navbar-brand" ! href "#blog" $ do
        img ! src "/img/favicon.png" ! A.style "height:32px" ! alt ""
        H.span ! class_ "title ml-2" $ "Dan Dart's Blog"
    H.div $
        ul ! class_ "navbar-nav px-3" $
            pageBlog blogPostLinks blogPosts

page :: Html -> Html -> Html
page blogPostLinks blogPosts = docTypeHtml ! lang "en-GB" $ do
    htmlHead descTitle keywords
    htmlHeader blogPostLinks blogPosts
