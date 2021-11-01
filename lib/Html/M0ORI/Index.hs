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
import           Text.Blaze.Html5            as H hiding (main)
import           Text.Blaze.Html5.Attributes as A

pageHamRadio ∷ WebsiteM Html
pageHamRadio = makePage "ham" "Ham Radio" defaultLayout defaultPage $ do
    p "I am a UK full-licenced radio amateur, and have been issued the callsign M0ORI."
    p $ do
        "My nearest radio club is "
        extLink "https://www.midsarc.org.uk/" "Mid-Somerset Amateur Radio Club"
        "."
    p $ do
        "I work on a "
        extLink "https://www.yaesu.com/indexVS.cfm?cmd=DisplayProducts&ProdCatID=102&encProdID=06014CD0AFA0702B25B12AB4DC9C0D27" "Yaesu FT-817"
        " (5W, all-mode HF, VHF, UHF transceiver)"
    p "You may sometimes find me on:"
    ul $ do
        li "FM on VHF/UHF in mid-Somerset, UK (IO81)."
        li "FT modes and occasionally PSK on HF"
    p $ extLink "https://www.qrzcq.com/call/M0ORI" "My QRZCQ page"
    br
    (H.div ! A.id "rigref-solar-widget")
        . (a ! href "https://www.hamqsl.com/solar.html" ! target "_blank")
        $ (img ! src "https://www.hamqsl.com/solar101vhf.php")

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
    pure . (docTypeHtml ! lang "en-GB") $ do
        head'
        header'

page404 ∷ WebsiteM Html
page404 = defaultPage404 descTitle keywords mempty