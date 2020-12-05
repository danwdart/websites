{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE UnicodeSyntax     #-}

module Html.Blog.Index (page) where

import           Data.Blog

import           Html.Common.Head

import           Html.Common.Page
import           Text.Blaze.Html5            as H hiding (main)
import           Text.Blaze.Html5.Attributes as A
import Html.Common.Header

pageBlog ∷ Html → Html → Html
pageBlog blogPostLinks blogPosts = makePage "blog" "Blog" customLayout defaultPage $ do
    H.div ! class_ "row" $ do
        H.div ! class_ "col-md-2 py-3 mb-3" $ blogPostLinks
        H.div ! class_ "col-md-8 py-3 mb-3 bg-light" $ blogPosts

htmlHeader ∷ Html → Html → Html
htmlHeader blogPostLinks blogPosts = makeHeader "#blog" "Dan Dart's Blog" mempty $ do
    extNav "https://dandart.co.uk" "Dan Dart"
    pageBlog blogPostLinks blogPosts
    dlNav "/atom.xml" "Atom Feed"

page ∷ Html → Html → Html
page blogPostLinks blogPosts = docTypeHtml ! lang "en-GB" $ do
    htmlHead descTitle keywords $ do
        link ! rel "alternate" ! type_ "application/atom+xml" ! A.title "Dan Dart's Blog" ! href "/atom.xml"
    htmlHeader blogPostLinks blogPosts
