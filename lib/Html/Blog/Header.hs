{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE UnicodeSyntax     #-}

module Html.Blog.Header where

import           Control.Monad.Trans.Reader
import           Data.Env.Types
import           Data.String
import           Html.Blog.Page.Blog
import           Html.Common.Header
import           Html.Common.Page
import           Text.Blaze.Html5           as H hiding (main)
import           Text.Pandoc.Highlighting

htmlHeader ∷ Html → Html → WebsiteM Html
htmlHeader blogPostLinks blogPosts = do
    urlDanDart' <- asks (urlDanDart . urls)
    pageBlog' <- pageBlog blogPostLinks blogPosts
    pure . makeHeader "#blog" "Dan Dart's Blog" mempty $ do
        extNav (textValue urlDanDart') "Dan Dart"
        pageBlog'
        dlNav "/atom.xml" "Atom Feed"
        H.style . fromString $ styleToCss haddock
