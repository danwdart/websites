{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE UnicodeSyntax     #-}

module Html.M0ORI.Index (page) where

import           Data.M0ORI

import           Html.Common.Head
import           Html.Common.Link

import           Text.Blaze.Html5            as H hiding (main)
import           Text.Blaze.Html5.Attributes as A
import Html.Common.Page
import Html.Common.Header

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

pageContact ∷ Html
pageContact = makePage "contact" "Contact" customLayout notDefaultPage $ do
    (H.div ! class_ "row") . (H.div ! class_ "col-lg-6 offset-lg-3 col-sm-12 col-md-12 col-xs-12 bg-light p-3 mb-3") $ (do
        p "If you would like to contact Dan, please use this form:"
        H.form ! action "https://formspree.io/website@m0ori.com" ! method "post" $ do
            H.div ! class_ "form-group" $ do
                H.label ! for "name" $ "Your name"
                input ! class_ "form-control" ! A.id "name" ! type_ "text" ! placeholder "John Smith" ! name "name" ! autocomplete "name"
            H.div ! class_ "form-group" $ do
                H.label ! for "email" $ "Your email"
                input ! class_ "form-control" ! A.id "email" ! type_ "email" ! placeholder "john@smith.com" ! name "email" ! autocomplete "email"
                small ! class_ "form-text text-muted" ! A.id "emailHelp" $ "I'll never share your email with anyone else."
            H.div ! class_ "form-group" $ do
                H.label ! for "subject" $ "Summary"
                input ! class_ "form-control" ! A.id "subject" ! type_ "text" ! placeholder "Greetings..." ! name "_subject"
            H.div ! class_ "form-group" $ do
                H.label ! for "message" $ "Your message"
                textarea ! class_ "form-control" ! A.id "message" ! placeholder "Hello!..." ! rows "10" ! name "message" $ mempty
            H.div ! class_ "form-group" $ input ! class_ "btn btn-primary" ! type_ "submit" ! value "Send")

htmlHeader ∷ Html
htmlHeader = makeHeader "" "M0ORI: Dan Dart" mempty $ do
    extNav "https://dandart.co.uk" "Dan Dart"
    pageHamRadio
    pageContact

page ∷ Html
page = docTypeHtml ! lang "en-GB" $ do
    htmlHead descTitle keywords mempty
    htmlHeader
