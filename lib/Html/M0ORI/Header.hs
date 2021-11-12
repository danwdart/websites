{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE UnicodeSyntax     #-}

module Html.M0ORI.Header where

import           Control.Monad.Trans.Reader
import           Data.Env
import           Data.Site.M0ORI
import           Html.Common.Contact
import           Html.Common.Error.NotFound
import           Html.Common.Head
import           Html.Common.Header
import           Html.Common.Link
import           Html.Common.Page
import           Html.M0ORI.Page.Contact
import           Html.M0ORI.Page.HamRadio
import           Text.Blaze.Html5            as H hiding (main)
import           Text.Blaze.Html5.Attributes as A

htmlHeader ∷ WebsiteM Html
htmlHeader = do
    urlDanDart' <- asks (urlDanDart . urls)
    pageHamRadio' <- pageHamRadio
    pageContact' <- pageContact
    pure . makeHeader "" "M0ORI: Dan Dart" mempty $ do
        extNav (textValue urlDanDart') "Dan Dart"
        pageHamRadio'
        pageContact'
