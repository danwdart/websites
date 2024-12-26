{-# LANGUAGE OverloadedStrings #-}

module Html.Common.Head where

import Control.Lens
import Control.Monad               (when)
import Control.Monad.Reader
import Data.Env.Types              as Env
import Data.Foldable
import Html.Common.Blog.Feed
import Html.Common.Blog.Types      as BlogTypes
import Html.Common.CSS
import Text.Blaze.Html5            as H hiding (main)
import Text.Blaze.Html5.Attributes as A

toOGProfileList ∷ OpenGraphProfile → [(AttributeValue, AttributeValue)]
toOGProfileList ogProfile = [
    ("profile:first_name", ogProfile ^. ogProfileFirstName . to textValue),
    ("profile:last_name", ogProfile ^. ogProfileLastName . to textValue),
    ("profile:username", ogProfile ^. ogProfileUsername . to textValue),
    ("profile:gender", ogProfile ^. ogProfileGender . to textValue)
    ]

toOGList ∷ OpenGraphInfo → [(AttributeValue, AttributeValue)]
toOGList OGWebsite = [
    ("og:type", "website")
    ]
toOGList (OGArticle ogArticle) = [
    ("og:type", "article"),
    ("article:published_time", ogArticle ^. ogArticlePublishedTime . to show . to stringValue),
    -- ("article:expiration_time", ...)
    -- ("article:author", -- ???
    ("article:section", ogArticle ^. ogArticleSection . to textValue)
    ]
        <> ogArticle ^. ogArticleTag . each . to (\t -> [("article:tag", textValue (BlogTypes.getTag t))])
        <> ogArticle ^. ogArticleAuthor . each . to toOGProfileList
    -- TODO: optional values
    --  ("article:modified_time", ogArticle ^. ogArticleModifiedTime . folded . to show . to stringValue),
toOGList (OGProfile ogProfile) = [
    ("og:type", "profile")
    ] <> ogProfile ^. to toOGProfileList

metas ∷ MonadReader Website m ⇒ m Html
metas = do
    title' <- view Env.title
    pageUrl' <- view pageUrl
    previewImgUrl' <- view previewImgUrl
    description' <- view description
    openGraphInfo' <- view openGraphInfo
    pure $ do
        meta ! charset "utf-8"
        traverse_ (\(aName, aCont) -> meta ! name aName ! content aCont) [
            ("title", textValue title'),
            ("url", stringValue $ show pageUrl'),
            ("description", textValue description'),
            ("robots", "index, follow"),
            ("theme-color", "#800080"),
            ("twitter:card", "summary_large_image"),
            -- ("twitter:site", "@..."),
            ("twitter:url", stringValue $ show pageUrl'),
            ("twitter:title", textValue title'),
            ("twitter:description", textValue description'),
            ("twitter:image", stringValue $ show previewImgUrl')
            ]
        meta ! name "viewport" ! content "width=device-width, initial-scale=1"
        traverse_ (\(aHE, aCont) -> meta ! httpEquiv aHE ! content aCont) [
            ("Content-Type", "text/html; charset=utf-8"),
            ("Who-is-awesome", "Raven"),
            ("X-UA-Compatible", "IE=edge,chrome=1")
            ]
        traverse_ (\(aProp, aCont) -> meta ! customAttribute "property" aProp ! content aCont) $ [
            ("og:title", textValue title'),
            ("og:url", stringValue $ show pageUrl'),
            ("og:description", textValue description'),
            ("og:locale", "en_GB"),
            ("og:image", stringValue $ show previewImgUrl')
            ] <> toOGList openGraphInfo'

htmlHead ∷ (MonadReader Website m) ⇒ m Html
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

