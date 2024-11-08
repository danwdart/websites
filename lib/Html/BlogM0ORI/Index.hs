{-# LANGUAGE OverloadedStrings #-}

module Html.BlogM0ORI.Index where

import Control.Monad.Reader
import Data.Env.Types
import Data.Site.BlogM0ORI
import Html.BlogM0ORI.Header
import Html.Common.Error.NotFound
import Html.Common.Head
import Text.Blaze.Html5            as H hiding (main)
import Text.Blaze.Html5.Attributes as A

extraHead ∷ Html
extraHead = link ! rel "alternate" ! type_ "application/atom+xml" ! A.title "M0ORI Blog" ! href "/atom.xml"

page ∷ MonadReader Website m ⇒ Html → Html → m Html
page blogPostLinks blogPosts = do
    header' <- htmlHeader blogPostLinks blogPosts
    head' <- htmlHead title' description' url' imgUrl extraHead
    pure . (docTypeHtml ! lang "en-GB") $ do
        head'
        header'

page404 ∷ MonadReader Website m ⇒ m Html
page404 = defaultPage404 title' description' url' imgUrl extraHead
