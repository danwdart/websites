{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE UnicodeSyntax     #-}

module Html.Blog.Index (page, page404) where

import           Data.Site.Blog

import Control.Monad.Trans.Reader
import Data.Env
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

htmlHeader ∷ Html → Html → WebsiteIO Html
htmlHeader blogPostLinks blogPosts = do
    urlDanDart' <- asks urlDanDart
    pure . makeHeader "#blog" "Dan Dart's Blog" mempty $ do
        extNav (textValue urlDanDart') "Dan Dart"
        pageBlog blogPostLinks blogPosts
        dlNav "/atom.xml" "Atom Feed"
        H.style . fromString $ styleToCss haddock

extraHead ∷ Html
extraHead = link ! rel "alternate" ! type_ "application/atom+xml" ! A.title "Dan Dart's Blog" ! href "/atom.xml"

page ∷ Html → Html → WebsiteIO Html
page blogPostLinks blogPosts = do
    header' <- htmlHeader blogPostLinks blogPosts
    head' <- htmlHead descTitle keywords extraHead
    visit' <- visit "blog"
    pure . (docTypeHtml ! lang "en-GB") $ do
        head'
        header'
        visit'

page404 ∷ WebsiteIO Html
page404 = do
    visit404 <- visit "blog404"
    defaultPage404 descTitle keywords $ do
        extraHead
        visit404
