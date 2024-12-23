{-# LANGUAGE OverloadedStrings #-}

module Html.BlogJolHarg.Header where

import Control.Lens
import Control.Monad.Reader
import Data.Env.Types
import Data.String
import Html.BlogJolHarg.Page.Blog
import Html.Common.Header
import Html.Common.Page
import Text.Blaze.Html5           as H hiding (main)
import Text.Pandoc.Highlighting

htmlHeader ∷ MonadReader Website m ⇒ Html → Html → Html → m Html
htmlHeader blogPostLinks blogTagLinks blogPosts = do
    urlJolHarg' <- view $ urls . urlJolHarg
    pageBlog' <- pageBlog blogPostLinks blogTagLinks blogPosts
    atomXml' <- view $ siteType . atomUrl . to show
    pure . makeHeader "/#blog" "JolHarg Blog" mempty $ do
        extNav (stringValue $ show urlJolHarg') "JolHarg"
        pageBlog'
        dlNav (stringValue atomXml') "Atom Feed"
        H.style . fromString $ styleToCss haddock
