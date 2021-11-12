{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE UnicodeSyntax     #-}

module Html.Blog.Index where

import           Data.Env
import           Data.Site.Blog
import           Data.String
import           Html.Blog.Header
import           Html.Common.Bootstrap
import           Html.Common.Error.NotFound
import           Html.Common.Head
import           Html.Common.Header
import           Html.Common.Page
import           Text.Blaze.Html5            as H hiding (main)
import           Text.Blaze.Html5.Attributes as A
import           Text.Pandoc.Highlighting

extraHead ∷ Html
extraHead = link ! rel "alternate" ! type_ "application/atom+xml" ! A.title "Dan Dart's Blog" ! href "/atom.xml"

page ∷ Html → Html → WebsiteM Html
page blogPostLinks blogPosts = do
    header' <- htmlHeader blogPostLinks blogPosts
    head' <- htmlHead descTitle keywords extraHead
    pure . (docTypeHtml ! lang "en-GB") $ do
        head'
        header'

page404 ∷ WebsiteM Html
page404 = defaultPage404 descTitle keywords extraHead
