{-# LANGUAGE OverloadedStrings #-}

module Html.BlogM0ORI.Index where

import Control.Monad.Reader
import Data.Env.Types
import Html.BlogM0ORI.Header
import Html.Common.Error.NotFound
import Html.Common.Head
import Text.Blaze.Html5            as H hiding (main)
import Text.Blaze.Html5.Attributes as A

page ∷ MonadReader Website m ⇒ Html → Html → Html → m Html
page blogPostLinks blogTagLinks blogPosts = do
    header' <- htmlHeader blogPostLinks blogTagLinks blogPosts
    head' <- htmlHead
    pure . (docTypeHtml ! lang "en-GB") $ do
        head'
        header'

page404 ∷ MonadReader Website m ⇒ m Html
page404 = defaultPage404
