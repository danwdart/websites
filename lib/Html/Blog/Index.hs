{-# LANGUAGE OverloadedStrings #-}

module Html.Blog.Index where

import Control.Monad.Reader
import Data.Env.Types
import Data.Site.Blog
import Html.Blog.Header
import Html.Common.Error.NotFound
import Html.Common.Head
import Text.Blaze.Html5            as H hiding (main)
import Text.Blaze.Html5.Attributes as A

extraHead ∷ Html
extraHead = link ! rel "alternate" ! type_ "application/atom+xml" ! A.title "Dan Dart's Blog" ! href "/atom.xml"

page ∷ MonadReader Website m ⇒ Html → Html → Html → (String -> String) -> m Html
page blogPostLinks blogTagLinks blogPosts titleModifier = do
    header' <- htmlHeader blogPostLinks blogTagLinks blogPosts
    head' <- htmlHead (titleModifier title') description' url' imgUrl extraHead
    pure . (docTypeHtml ! lang "en-GB") $ do
        head'
        header'

page404 ∷ MonadReader Website m ⇒ m Html
page404 = defaultPage404 title' description' url' imgUrl extraHead
