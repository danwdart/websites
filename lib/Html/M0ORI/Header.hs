{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE UnicodeSyntax     #-}

module Html.M0ORI.Header where

import           Control.Monad.Reader
import           Data.Env.Types
import           Html.Common.Header
import           Html.Common.Page
import           Html.M0ORI.Page.Contact
import           Html.M0ORI.Page.HamRadio
import           Text.Blaze.Html5         as H hiding (main)

htmlHeader ∷ MonadReader Website m ⇒ m Html
htmlHeader = do
    urlDanDart' <- asks (urlDanDart . urls)
    pageHamRadio' <- pageHamRadio
    pageContact' <- pageContact
    pure . makeHeader "" "M0ORI: Dan Dart" mempty $ do
        extNav (textValue urlDanDart') "Dan Dart"
        pageHamRadio'
        pageContact'
