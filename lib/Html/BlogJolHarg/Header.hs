{-# LANGUAGE OverloadedStrings #-}

module Html.BlogJolHarg.Header where

import Control.Monad.Reader
import Data.Env.Types
import Data.String
import Html.BlogJolHarg.Page.Blog
import Html.Common.Header
import Html.Common.Page
import Text.Blaze.Html5           as H hiding (main)
import Text.Pandoc.Highlighting

htmlHeader ∷ MonadReader Website m ⇒ Html → Html → m Html
htmlHeader blogPostLinks blogPosts = do
    urlJolHarg' <- asks (urlJolHarg . urls)
    pageBlog' <- pageBlog blogPostLinks blogPosts
    pure . makeHeader "#blog" "JolHarg Blog" mempty $ do
        extNav (textValue urlJolHarg') "JolHarg"
        pageBlog'
        dlNav "/atom.xml" "Atom Feed"
        H.style . fromString $ styleToCss haddock
