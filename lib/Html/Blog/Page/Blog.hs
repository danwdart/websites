{-# LANGUAGE OverloadedStrings #-}

module Html.Blog.Page.Blog where

import           Control.Monad.Reader
import           Data.Env.Types
import           Html.Common.Bootstrap
import           Html.Common.Page
import           Text.Blaze.Html5            as H hiding (main)
import           Text.Blaze.Html5.Attributes as A

pageBlog ∷ MonadReader Website m ⇒ Html → Html → m Html
pageBlog blogPostLinks blogPosts = makePage "blog" "Blog" customLayout defaultPage $ do
    row $ do
        H.div ! class_ "col-md-2 py-3 mb-3" $ blogPostLinks
        H.div ! class_ "col-md-8 py-3 mb-3 bg-light" $ blogPosts
