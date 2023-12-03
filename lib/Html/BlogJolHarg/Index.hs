{-# LANGUAGE OverloadedStrings #-}

module Html.BlogJolHarg.Index where

import Control.Monad.Reader
import Data.Env.Types
import Data.Site.BlogJolHarg
import Html.BlogJolHarg.Header
import Html.Common.Error.NotFound
import Html.Common.Head
import Text.Blaze.Html5            as H hiding (main)
import Text.Blaze.Html5.Attributes as A

extraHead ∷ Html
extraHead = link ! rel "alternate" ! type_ "application/atom+xml" ! A.title "JolHarg Blog" ! href "/atom.xml"

page ∷ MonadReader Website m ⇒ Html → Html → m Html
page blogPostLinks blogPosts = do
    header' <- htmlHeader blogPostLinks blogPosts
    head' <- htmlHead title' description' url' imgUrl keywords extraHead
    pure . (docTypeHtml ! lang "en-GB") $ do
        head'
        header'

page404 ∷ MonadReader Website m ⇒ m Html
page404 = defaultPage404 title' description' url' imgUrl keywords extraHead
