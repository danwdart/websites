{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE UnicodeSyntax     #-}

module Html.M0ORI.Index (page, page404) where

import           Data.Site.M0ORI
import Data.Env
import Control.Monad.Trans.Reader

import           Html.Common.Head
import           Html.Common.Link

import           Html.Common.Contact
import           Html.Common.Error.NotFound
import           Html.Common.Header
import           Html.Common.Page
import           Html.Common.Visit
import           Text.Blaze.Html5            as H hiding (main)
import           Text.Blaze.Html5.Attributes as A

pageHamRadio ∷ Html
pageHamRadio = makePage "ham" "Ham Radio" defaultLayout defaultPage $ do
    p "I am a UK full-licenced radio amateur, and have been issued the callsign M0ORI."
    p $ do
        "My nearest radio club is "
        extLink "https://www.midsarc.org.uk/" "Mid-Somerset Amateur Radio Club"
        "."
    p "I own the following types of radio:"
    ul $ do
        li $ do
            extLink "https://www.yaesu.com/indexVS.cfm?cmd=DisplayProducts&ProdCatID=102&encProdID=06014CD0AFA0702B25B12AB4DC9C0D27" "Yaesu FT-817"
            " (5W, all-mode HF, VHF, UHF transceiver)"
        li $ do
            extLink "https://www.amazon.co.uk/BaoFeng-UV-5R-Radio-Walkie-Talkie/dp/B072HXQ5GG" "Baofeng UV-5R 8W"
            " (8W, FM, VHF and UHF transceiver)"
        li $ do
            extLink "https://baofengtech.com/uv82" "Baofeng UV-82"
            " (5W, FM, VHF and UHF transceiver)"
        li $ do
            extLink "http://www.uv3r.com/" "Baofeng UV-3R"
            " (2W, FM, VHF and UHF transceiver)"
        li $ do
            extLink "https://www.eham.net/reviews/detail/7627" "Tecsun PL-600"
            " (HF receiver)"
    p "You may sometimes find me on:"
    ul $ do
        li "FM in mid-Somerset, UK (IO81)."
        li "PSK on usually 20m"
        li "JT modes on usually 20m"
    p $ extLink "https://www.qrzcq.com/call/M0ORI" "My QRZCQ page"
    br
    (H.div ! A.id "rigref-solar-widget")
        . (a ! href "https://rigreference.com/solar" ! target "_blank")
        $ (img ! src "https://rigreference.com/solar/img/tall")

pageContact ∷ Html
pageContact = makePage "contact" "Contact" contactLayout notDefaultPage $ do
    p "If you would like to contact Dan, please use this form:"
    contactForm "website@m0ori.com" emailHelpSingular "Greetings..." "Hello!..."

htmlHeader ∷ WebsiteIO Html
htmlHeader = do
    urlDanDart' <- asks urlDanDart
    pure . makeHeader "" "M0ORI: Dan Dart" mempty $ do
        extNav (textValue urlDanDart') "Dan Dart"
        pageHamRadio
        pageContact

page ∷ WebsiteIO Html
page = do
    header' <- htmlHeader
    head' <- htmlHead descTitle keywords mempty
    pure . (docTypeHtml ! lang "en-GB") $ do
        head'
        header'
        visit "m0ori"

page404 ∷ WebsiteIO Html
page404 = defaultPage404 descTitle keywords $ visit "m0ori404"
