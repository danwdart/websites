{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE UnicodeSyntax     #-}

module Html.DanDart.Index (page, page404) where

import           Data.Env
import           Data.Site.DanDart
import           Html.Common.Audio
import Html.Common.Bootstrap
import           Html.Common.Contact
import           Html.Common.Error.NotFound
import           Html.Common.Head
import           Html.Common.Header
import           Html.Common.Link
import           Html.Common.Page
import           Html.Common.Shortcuts
import           Html.Common.Social
import           Html.Common.Visit
import           Text.Blaze.Html5            as H hiding (main)
import           Text.Blaze.Html5.Attributes as A

pageIntro ∷ WebsiteM Html
pageIntro = makePage "intro" "Intro" defaultLayout defaultPage $ do
    -- md

pageCharacters ∷ WebsiteM Html
pageCharacters = makePage "characters" "Characters" defaultLayout notDefaultPage $ do
    -- md

pageFavourites ∷ WebsiteM Html
pageFavourites = makePage "favourites" "Favourites" defaultLayout notDefaultPage $ do
    -- md

pageHealth ∷ WebsiteM Html
pageHealth = makePage "health" "Health" defaultLayout notDefaultPage $ do
    -- md

pageMusic ∷ WebsiteM Html
pageMusic = makePage "music" "Music" defaultLayout notDefaultPage $ do
    -- md

pageMaths ∷ WebsiteM Html
pageMaths = makePage "maths" "Maths" defaultLayout notDefaultPage $ do
    -- md
    
pageOrigami ∷ WebsiteM Html
pageOrigami = makePage "origami" "Origami" defaultLayout notDefaultPage $ do
    -- md

pageAbout ∷ WebsiteM Html
pageAbout = makePage "about" "About" defaultLayout notDefaultPage $ do
    -- md
    
pageHamRadio ∷ WebsiteM Html
pageHamRadio = do
    urlHamRadio' <- asks urlHamRadio
    pure $ extNav (textValue urlHamRadio') "Ham Radio"

pageSoftware ∷ WebsiteM Html
pageSoftware = do
    urlJolHarg' <- asks urlJolHarg
    pure $ extNav (textValue urlJolHarg') "Software"

pageBlog ∷ WebsiteM Html
pageBlog = do
    urlBlog' <- asks urlBlog
    pure $ extNav (textValue urlBlog') "Blog"

pageReviews ∷ WebsiteM Html
pageReviews = do
    urlMadHacker' <- asks urlMadHacker
    pure $ extNav (textValue urlMadHacker') "Reviews"

pageContact ∷ WebsiteM Html
pageContact = do
    contactForm' <- contactForm "website@dandart.co.uk" emailHelpSingular "Greetings..." "Hello!..."
    makePage "contact" "Contact" contactLayout notDefaultPage $ do
        contactForm'

socialIcons ∷ Html
socialIcons = divClass "row social-row" . divClass "text-right social-inside" $ (do
   -- from yaml

htmlHeader ∷ WebsiteM Html
htmlHeader = do
    pages <- do
        --stuff
    pure . makeHeader "#intro" "Dan Dart" socialIcons $ pages

page ∷ WebsiteM Html
page = do
    header' <- htmlHeader
    head' <- htmlHead descTitle keywords mempty
    visit' <- visit "dandart"
    pure $ do
        docTypeHtml ! lang "en-GB" $ do
            head'
            header'
            visit'

page404 ∷ WebsiteM Html
page404 = do
    visit' <- visit "dandart404"
    defaultPage404 descTitle keywords visit'
