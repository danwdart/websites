{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE UnicodeSyntax     #-}

module Html.M0ORI.Index where

import           Data.Env
import           Data.Site.M0ORI
import           Html.Common.Contact
import           Html.Common.Error.NotFound
import           Html.Common.Head
import           Html.Common.Header
import           Html.Common.Link
import           Html.Common.Page
import           Html.M0ORI.Header
import           Text.Blaze.Html5            as H hiding (main)
import           Text.Blaze.Html5.Attributes as A

page ∷ WebsiteM Html
page = do
    header' <- htmlHeader
    head' <- htmlHead descTitle keywords mempty
    pure . (docTypeHtml ! lang "en-GB") $ do
        head'
        header'

page404 ∷ WebsiteM Html
page404 = defaultPage404 descTitle keywords mempty
