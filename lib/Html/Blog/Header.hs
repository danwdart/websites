{-# LANGUAGE OverloadedStrings #-}

module Html.Blog.Header where

import Control.Monad.Reader
import Data.Env.Types
import Data.String
import Data.Text.Encoding
import Html.Blog.Page.Blog
import Html.Common.Header
import Html.Common.Page
import Text.Blaze.Html5         as H hiding (main)
import Text.Pandoc.Highlighting

htmlHeader ∷ MonadReader Website m ⇒ Html → Html → Html → m Html
htmlHeader blogPostLinks blogTagLinks blogPosts = do
    urlDanDart' <- asks (urlDanDart . urls)
    pageBlog' <- pageBlog blogPostLinks blogTagLinks blogPosts
    pure . makeHeader "/#blog" "Dan Dart's Blog" mempty $ do
        extNav (textValue $ decodeUtf8 urlDanDart') "Dan Dart"
        pageBlog'
        dlNav "/atom.xml" "Atom Feed"
        H.style . fromString $ styleToCss haddock
