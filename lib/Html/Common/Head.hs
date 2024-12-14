{-# LANGUAGE OverloadedStrings #-}

module Html.Common.Head where

import Control.Lens
import Control.Monad               (when)
import Control.Monad.Reader
import Data.Env.Types              as Env
import Data.Foldable
import Data.Text.Encoding
import Html.Common.Blog.Feed
import Html.Common.CSS
import Text.Blaze.Html5            as H hiding (main)
import Text.Blaze.Html5.Attributes as A

metas ∷ MonadReader Website m => m Html
metas = do
    title' <- view Env.title
    pageUrl' <- view pageUrl
    imgUrl' <- view imgUrl
    description' <- view description
    pure $ do
        meta ! charset "utf-8"
        traverse_ (\(aName, aCont) -> meta ! name aName ! content aCont) [
            ("title", textValue title'),
            ("url", textValue $ decodeUtf8 pageUrl'),
            ("description", textValue description'),
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
            ("og:url", textValue $ decodeUtf8 pageUrl'),
            ("og:description", textValue description'),
            ("og:locale", "en_GB"),
            ("og:image", textValue $ decodeUtf8 imgUrl'),
            ("twitter:card", "summary_large_image"),
            ("twitter:url", textValue $ decodeUtf8 pageUrl'),
            ("twitter:title", textValue title'),
            ("twitter:description", textValue description'),
            ("twitter:image", textValue $ decodeUtf8 imgUrl')
            ]

htmlHead ∷ (MonadReader Website m) => m Html
htmlHead = do
    title' <- view Env.title
    metas' <- metas
    livereload' <- view livereload
    extraHead' <- extraHead
    pure . H.head $ do
        H.title $ toHtml title'
        metas'
        commonCSS
        extraHead'
        when livereload' . (script ! src "/js/livereload.js") $ mempty

