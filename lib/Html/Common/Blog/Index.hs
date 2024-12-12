{-# LANGUAGE OverloadedStrings #-}

module Html.Common.Blog.Index where

import Control.Monad.Reader
import Data.Env.Types
import Html.Common.Blog.Feed
import Html.Common.Error.NotFound
import Html.Common.Head
import Text.Blaze.Html5            as H hiding (main)
import Text.Blaze.Html5.Attributes as A

page ∷ MonadReader Website m ⇒ Html → Html → Html → (Text -> Text) -> m Html
page blogPostLinks blogTagLinks blogPosts titleModifier = do
    header' <- htmlHeader blogPostLinks blogTagLinks blogPosts
    head' <- htmlHead (titleModifier title') description' url' imgUrl (extraHead title' "/atom.xml")
    pure . (docTypeHtml ! lang "en-GB") $ do
        head'
        header'

page404 ∷ MonadReader Website m ⇒ m Html
page404 = defaultPage404 title' description' url' imgUrl extraHead
