{-# LANGUAGE OverloadedStrings #-}

module Html.M0ORI.Header where

import Control.Lens
import Control.Monad.Reader
import Data.Env.Types
import Data.Text.Encoding
import Html.Common.Header
import Html.Common.Page
import Html.M0ORI.Page.Contact
import Html.M0ORI.Page.HamRadio
import Text.Blaze.Html5         as H hiding (main)

htmlHeader ∷ MonadReader Website m ⇒ m Html
htmlHeader = do
    urlDanDart' <- view $ urls . urlDanDart
    pageHamRadio' <- pageHamRadio
    pageContact' <- pageContact
    urlBlogM0ORI' <- view $ urls . urlBlogHamRadio
    pure . makeHeader "" "M0ORI: Dan Dart" mempty $ do
        extNav (stringValue $ show urlDanDart') "Dan Dart"
        pageHamRadio'
        extNav (stringValue $ show urlBlogM0ORI') "Blog"
        pageContact'
