{-# LANGUAGE OverloadedStrings #-}

module Html.Blog.Header where

import Control.Lens
import Control.Monad.Reader
import Data.Env.Types
import Data.String
import Html.Blog.Page.Blog
import Html.Common.Header
import Html.Common.Page
import Text.Blaze.Html5         as H hiding (main)
import Text.Pandoc.Highlighting

htmlHeader ∷ MonadReader Website m ⇒ Html → Html → Html → m Html
htmlHeader blogPostLinks blogTagLinks blogPosts = do
    urlDanDart' <- view $ urls . urlDanDart
    pageBlog' <- pageBlog blogPostLinks blogTagLinks blogPosts
    atomXml' <- view $ siteType . atomUrl . to show
    pure . makeHeader "/#blog" "Dan Dart's Blog" mempty $ do
        extNav (stringValue $ show urlDanDart') "Dan Dart"
        pageBlog'
        dlNav (stringValue atomXml') "Atom Feed"
        H.style . fromString $ styleToCss haddock
