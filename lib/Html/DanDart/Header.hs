{-# LANGUAGE OverloadedStrings #-}

module Html.DanDart.Header where

import Control.Lens
import Control.Monad.Reader
import Data.Env.Types
import Data.Foldable.Monoid
import Data.Text.Encoding
import Html.Common.Header
import Html.Common.Page
import Html.DanDart.Page.About
import Html.DanDart.Page.Characters
import Html.DanDart.Page.Contact
import Html.DanDart.Page.Favourites
import Html.DanDart.Page.Health
import Html.DanDart.Page.Intro
import Html.DanDart.Page.Maths
import Html.DanDart.Page.Music
import Html.DanDart.Page.Origami
import Html.DanDart.Social
import Text.Blaze.Html5             as H hiding (main)

linkHamRadio ∷ (MonadReader Website m) ⇒ m Html
linkHamRadio = do
    urlHamRadio' <- view $ urls . urlHamRadio
    pure $ extNav (textValue $ decodeUtf8 urlHamRadio') "Ham Radio"

linkSoftware ∷ (MonadReader Website m) ⇒ m Html
linkSoftware = do
    urlJolHarg' <- view $ urls . urlJolHarg
    pure $ extNav (textValue $ decodeUtf8 urlJolHarg') "Software"

linkBlog ∷ (MonadReader Website m) ⇒ m Html
linkBlog = do
    urlBlog' <- view $ urls . urlBlog
    pure $ extNav (textValue $ decodeUtf8 urlBlog') "Blog"

linkReviews ∷ (MonadReader Website m) ⇒ m Html
linkReviews = do
    urlMadHacker' <- view $ urls . urlMadHacker
    pure $ extNav (textValue $ decodeUtf8 urlMadHacker') "Reviews"

htmlHeader ∷ (MonadReader Website m) ⇒ m Html
htmlHeader = do
    pages <- foldA [
        pageIntro,
        pageCharacters,
        pageFavourites,
        linkHamRadio,
        pageHealth,
        pageMusic,
        pageMaths,
        pageOrigami,
        pageAbout,
        linkSoftware,
        linkBlog,
        linkReviews,
        pageContact
        ]
    pure . makeHeader "#intro" "Dan Dart" socialIcons $ pages
