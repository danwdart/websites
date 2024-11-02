{-# LANGUAGE OverloadedStrings #-}

module Html.DanDart.Header where

import Control.Monad.Reader
import Data.Env.Types
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
    urlHamRadio' <- asks (urlHamRadio . urls)
    pure $ extNav (textValue urlHamRadio') "Ham Radio"

linkSoftware ∷ (MonadReader Website m) ⇒ m Html
linkSoftware = do
    urlJolHarg' <- asks (urlJolHarg . urls)
    pure $ extNav (textValue urlJolHarg') "Software"

linkBlog ∷ (MonadReader Website m) ⇒ m Html
linkBlog = do
    urlBlog' <- asks (urlBlog . urls)
    pure $ extNav (textValue urlBlog') "Blog"

linkReviews ∷ (MonadReader Website m) ⇒ m Html
linkReviews = do
    urlMadHacker' <- asks (urlMadHacker . urls)
    pure $ extNav (textValue urlMadHacker') "Reviews"

htmlHeader ∷ (MonadReader Website m) ⇒ m Html
htmlHeader = do
    pageIntro' <- pageIntro
    pageCharacters' <- pageCharacters
    pageFavourites' <- pageFavourites
    linkHamRadio' <- linkHamRadio
    pageHealth' <- pageHealth
    pageMusic' <- pageMusic
    pageMaths' <- pageMaths
    pageOrigami' <- pageOrigami
    pageAbout' <- pageAbout
    linkSoftware' <- linkSoftware
    linkBlog' <- linkBlog
    linkReviews' <- linkReviews
    pageContact' <- pageContact

    let pages = pageIntro' <>
            pageCharacters' <>
            pageFavourites' <>
            linkHamRadio' <>
            pageHealth' <>
            pageMusic' <>
            pageMaths' <>
            pageOrigami' <>
            pageAbout' <>
            linkSoftware' <>
            linkBlog' <>
            linkReviews' <>
            pageContact'

    pure . makeHeader "#intro" "Dan Dart" socialIcons $ pages
