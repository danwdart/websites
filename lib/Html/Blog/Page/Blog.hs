{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE UnicodeSyntax     #-}

module Html.Blog.Page.Blog where

import           Data.Env
import           Data.Site.Blog
import           Data.String
import           Html.Common.Bootstrap
import           Html.Common.Error.NotFound
import           Html.Common.Head
import           Html.Common.Header
import           Html.Common.Page
import           Text.Blaze.Html5            as H hiding (main)
import           Text.Blaze.Html5.Attributes as A
import           Text.Pandoc.Highlighting

pageBlog ∷ Html → Html → WebsiteM Html
pageBlog blogPostLinks blogPosts = makePage "blog" "Blog" customLayout defaultPage $ do
    row $ do
        H.div ! class_ "col-md-2 py-3 mb-3" $ blogPostLinks
        H.div ! class_ "col-md-8 py-3 mb-3 bg-light" $ blogPosts