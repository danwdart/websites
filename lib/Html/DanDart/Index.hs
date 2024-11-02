{-# LANGUAGE OverloadedStrings #-}

module Html.DanDart.Index where

import Control.Monad.Reader
import Data.Env.Types
import Data.Site.DanDart
import Html.Common.Error.NotFound
import Html.Common.Head
import Html.DanDart.Header
import Text.Blaze.Html5            as H hiding (main)
import Text.Blaze.Html5.Attributes as A

page ∷ (MonadReader Website m) ⇒ m Html
page = do
    header' <- htmlHeader
    head' <- htmlHead title' description' url' imgUrl mempty
    pure $ do
        docTypeHtml ! lang "en-GB" $ do
            head'
            header'

page404 ∷ (MonadReader Website m) ⇒ m Html
page404 = defaultPage404 title' description' url' imgUrl mempty
