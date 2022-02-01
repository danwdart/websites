{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE UnicodeSyntax     #-}

module Html.DanDart.Header where

import           Control.Monad.Trans.Reader
import           Data.Env.Types
import           Html.Common.Header
import           Html.Common.Page
import           Html.DanDart.Page.About
import           Html.DanDart.Page.Characters
import           Html.DanDart.Page.Contact
import           Html.DanDart.Page.Favourites
import           Html.DanDart.Page.Health
import           Html.DanDart.Page.Intro
import           Html.DanDart.Page.Maths
import           Html.DanDart.Page.Morals
import           Html.DanDart.Page.Music
import           Html.DanDart.Page.Origami
import           Html.DanDart.Social
import           Text.Blaze.Html5             as H hiding (main)

linkHamRadio ∷ WebsiteM Html
linkHamRadio = do
    urlHamRadio' <- asks (urlHamRadio . urls)
    pure $ extNav (textValue urlHamRadio') "Ham Radio"

linkSoftware ∷ WebsiteM Html
linkSoftware = do
    urlJolHarg' <- asks (urlJolHarg . urls)
    pure $ extNav (textValue urlJolHarg') "Software"

linkBlog ∷ WebsiteM Html
linkBlog = do
    urlBlog' <- asks (urlBlog . urls)
    pure $ extNav (textValue urlBlog') "Blog"

linkReviews ∷ WebsiteM Html
linkReviews = do
    urlMadHacker' <- asks (urlMadHacker . urls)
    pure $ extNav (textValue urlMadHacker') "Reviews"

htmlHeader ∷ WebsiteM Html
htmlHeader = do
    pages <- do
        pageIntro <>
            pageCharacters <>
            pageFavourites <>
            linkHamRadio <>
            pageHealth <>
            pageMusic <>
            pageMaths <>
            pageMorals <>
            pageOrigami <>
            pageAbout <>
            linkSoftware <>
            linkBlog <>
            linkReviews <>
            pageContact
    pure . makeHeader "#intro" "Dan Dart" socialIcons $ pages
