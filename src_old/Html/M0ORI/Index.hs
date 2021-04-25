{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE UnicodeSyntax     #-}

module Html.M0ORI.Index (page, page404) where

import           Data.Env
import           Data.Site.M0ORI

import           Html.Common.Head
import           Html.Common.Link

import           Html.Common.Contact
import           Html.Common.Error.NotFound
import           Html.Common.Header
import           Html.Common.Page
import           Html.Common.Visit
import           Text.Blaze.Html5            as H hiding (main)
import           Text.Blaze.Html5.Attributes as A

pageHamRadio ∷ WebsiteM Html
pageHamRadio = makePage "ham" "Ham Radio" defaultLayout defaultPage $ do
    -- md
    
pageContact ∷ WebsiteM Html
pageContact = do
    contactForm' <- contactForm "website@m0ori.com" emailHelpSingular "Greetings..." "Hello!..."
    makePage "contact" "Contact" contactLayout notDefaultPage $ do
        contactForm'

htmlHeader ∷ WebsiteM Html
htmlHeader = do
    urlDanDart' <- asks urlDanDart
    pageHamRadio' <- pageHamRadio
    pageContact' <- pageContact
    pure . makeHeader "" "M0ORI: Dan Dart" mempty $ do
        extNav (textValue urlDanDart') "Dan Dart"
        pageHamRadio'
        pageContact'

page ∷ WebsiteM Html
page = do
    header' <- htmlHeader
    head' <- htmlHead descTitle keywords mempty
    visit' <- visit "m0ori"
    pure . (docTypeHtml ! lang "en-GB") $ do
        head'
        header'
        visit'

page404 ∷ WebsiteM Html
page404 = do
    visit' <- visit "m0ori404"
    defaultPage404 descTitle keywords visit'
