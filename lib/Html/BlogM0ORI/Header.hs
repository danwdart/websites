{-# LANGUAGE OverloadedStrings #-}

module Html.BlogM0ORI.Header where

import Control.Monad.Reader
import Data.Env.Types
import Data.String
import Html.BlogM0ORI.Page.Blog
import Html.Common.Header
import Html.Common.Page
import Text.Blaze.Html5         as H hiding (main)
import Text.Pandoc.Highlighting

htmlHeader ∷ MonadReader Website m ⇒ Html → Html → Html → m Html
htmlHeader blogPostLinks blogTagLinks blogPosts = do
    urlHamRadio' <- asks (urlHamRadio . urls)
    pageBlog' <- pageBlog blogPostLinks blogTagLinks blogPosts
    pure . makeHeader "/#blog" "M0ORI Blog" mempty $ do
        extNav (textValue urlHamRadio') "M0ORI"
        pageBlog'
        dlNav "/atom.xml" "Atom Feed"
        H.style . fromString $ styleToCss haddock
