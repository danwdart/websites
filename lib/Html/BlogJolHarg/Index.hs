{-# LANGUAGE OverloadedStrings #-}

module Html.BlogJolHarg.Index where

import Control.Monad.Reader
import Data.Env.Types as Env
import Html.BlogJolHarg.Header
import Html.Common.Blog.Feed
import Html.Common.Error.NotFound
import Html.Common.Head
import Text.Blaze.Html5            as H hiding (main)
import Text.Blaze.Html5.Attributes as A

page ∷ MonadReader Website m ⇒ Html → Html → Html -> m Html
page blogPostLinks blogTagLinks blogPosts = do
    title' <- asks Env.title
    header' <- htmlHeader blogPostLinks blogTagLinks blogPosts
    head' <- htmlHead (extraHead title' "/atom.xml")
    pure . (docTypeHtml ! lang "en-GB") $ do
        head'
        header'

page404 ∷ MonadReader Website m ⇒ m Html
page404 = do
    title' <- asks Env.title
    defaultPage404 (extraHead title' "/atom.xml")
