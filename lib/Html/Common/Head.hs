{-# LANGUAGE OverloadedStrings #-}

module Html.Common.Head where

import Control.Monad               (when)
import Control.Monad.Reader
import Data.Env.Types
import Data.Foldable
import Data.String
import Html.Common.CSS
import Text.Blaze.Html5            as H hiding (main)
import Text.Blaze.Html5.Attributes as A

metas ∷ String → String → String → String → Html
metas title' description url imgUrl = do
    meta ! charset "utf-8"
    traverse_ (\(aName, aCont) -> meta ! name aName ! content aCont) [
        ("title", fromString title'),
        ("url", fromString url),
        ("description", fromString description),
        ("theme-color", "#800080")
        ]
    meta ! name "viewport" ! content "width=device-width, initial-scale=1"
    traverse_ (\(aHE, aCont) -> meta ! httpEquiv aHE ! content aCont) [
        ("Content-Type", "text/html; charset=utf-8"),
        ("Who-is-awesome", "Raven"),
        ("X-UA-Compatible", "IE=edge,chrome=1")
        ]
    traverse_ (\(aProp, aCont) -> meta ! customAttribute "property" aProp ! content aCont) [
        ("og:type", "website"), -- https://ogp.me/#types
        ("og:url", fromString url),
        ("og:description", fromString description),
        ("og:locale", "en_GB"),
        ("og:image", fromString imgUrl),
        ("twitter:card", "summary_large_image"),
        ("twitter:url", fromString url),
        ("twitter:title", fromString title'),
        ("twitter:description", fromString description),
        ("twitter:image", fromString imgUrl)
        ]

htmlHead ∷ (MonadReader Website m) ⇒ String → String → String → String → Html → m Html
htmlHead title' description url imgUrl extraHead = do
    livereload' <- asks livereload
    pure . H.head $ do
        H.title $ toHtml title'
        metas title' description url imgUrl
        commonCSS
        extraHead
        when livereload' . (script ! src "/js/livereload.js") $ mempty

