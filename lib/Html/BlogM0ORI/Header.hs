{-# LANGUAGE OverloadedStrings #-}

module Html.BlogM0ORI.Header where

import Control.Lens
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
    urlHamRadio' <- view $ urls . urlHamRadio
    pageBlog' <- pageBlog blogPostLinks blogTagLinks blogPosts
    atomXml' <- view $ siteType . atomUrl . to show
    pure . makeHeader "/#blog" "M0ORI Blog" mempty $ do
        extNav (stringValue $ show urlHamRadio') "M0ORI"
        pageBlog'
        dlNav (toValue atomXml') "Atom Feed"
        H.style . fromString $ styleToCss haddock
