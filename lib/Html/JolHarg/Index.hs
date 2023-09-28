{-# LANGUAGE OverloadedStrings #-}

module Html.JolHarg.Index where

import Control.Monad.Reader
import Data.Env.Types
import Data.Site.JolHarg
import Html.Common.Error.NotFound
import Html.Common.GitHub
import Html.Common.Head
import Html.JolHarg.Header
import Text.Blaze.Html5            as H hiding (main)
import Text.Blaze.Html5.Attributes as A

page ∷ (MonadReader [Repo] n, MonadReader Website m) ⇒ n (m Html)
page = do
    header' <- htmlHeader
    pure $ do
        head' <- htmlHead descTitle keywords mempty
        header'' <- header'
        pure . (docTypeHtml ! lang "en-GB") $ do
            head'
            header''

page404 ∷ MonadReader Website m ⇒ m Html
page404 = defaultPage404 descTitle keywords mempty
