{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE UnicodeSyntax     #-}

module Html.JolHarg.Index where

import           Control.Monad.Reader
import           Data.Env
import           Data.Site.JolHarg
import           Html.Common.Bootstrap
import           Html.Common.Card
import           Html.Common.Contact
import           Html.Common.Error.NotFound
import           Html.Common.GitHub
import           Html.Common.Head
import           Html.Common.Header
import           Html.Common.Link
import           Html.Common.Page
import           Html.JolHarg.Header
import           Text.Blaze.Html5            as H hiding (main)
import           Text.Blaze.Html5.Attributes as A

page ∷ Reader [Repo] (WebsiteM Html)
page = do
    header' <- htmlHeader
    pure $ do
        head' <- htmlHead descTitle keywords mempty
        header'' <- header'
        pure . (docTypeHtml ! lang "en-GB") $ do
            head'
            header''

page404 ∷ WebsiteM Html
page404 = defaultPage404 descTitle keywords mempty
