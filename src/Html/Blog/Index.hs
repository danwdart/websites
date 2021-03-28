{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE UnicodeSyntax     #-}

module Html.Blog.Index (page, page404) where

import           Data.Site.Blog

import           Data.String
import           Html.Common.Head
import           Html.Common.Bootstrap
import           Html.Common.Error.NotFound
import           Html.Common.Header
import           Html.Common.Page
import           Html.Common.Visit
import           Text.Blaze.Html5            as H hiding (main)
import           Text.Blaze.Html5.Attributes as A

import           Text.Pandoc.Highlighting

pageBlog ∷ Html → Html → Html
pageBlog blogPostLinks blogPosts = makePage "blog" "Blog" customLayout defaultPage $ do
    row $ do
        H.div ! class_ "col-md-2 py-3 mb-3" $ blogPostLinks
        H.div ! class_ "col-md-8 py-3 mb-3 bg-light" $ blogPosts

htmlHeader ∷ Bool → Html → Html → Html
htmlHeader dev blogPostLinks blogPosts = makeHeader "#blog" "Dan Dart's Blog" mempty $ do
    extNav (if dev then "http://dandart.localhost:8080" else "https://dandart.co.uk") "Dan Dart"
    pageBlog blogPostLinks blogPosts
    dlNav "/atom.xml" "Atom Feed"
    H.style . fromString $ styleToCss haddock

extraHead ∷ Html
extraHead = link ! rel "alternate" ! type_ "application/atom+xml" ! A.title "Dan Dart's Blog" ! href "/atom.xml"


page ∷ Bool → Html → Html → Html
page dev blogPostLinks blogPosts = docTypeHtml ! lang "en-GB" $ do
    htmlHead dev descTitle keywords extraHead
    htmlHeader dev blogPostLinks blogPosts
    visit "blog"

page404 ∷ Html
page404 = defaultPage404 descTitle keywords $ do
    extraHead
    visit "blog404"
