{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE UnicodeSyntax     #-}

module Html.DanDart.Index where

import           Data.Env.Types
import           Data.Site.DanDart
import           Html.Common.Error.NotFound
import           Html.Common.Head
import           Html.DanDart.Header
import           Text.Blaze.Html5            as H hiding (main)
import           Text.Blaze.Html5.Attributes as A

page ∷ WebsiteM Html
page = do
    header' <- htmlHeader
    head' <- htmlHead descTitle keywords mempty
    pure $ do
        docTypeHtml ! lang "en-GB" $ do
            head'
            header'

page404 ∷ WebsiteM Html
page404 = defaultPage404 descTitle keywords mempty
