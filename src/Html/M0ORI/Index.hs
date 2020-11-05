{-# LANGUAGE OverloadedStrings #-}

module Html.M0ORI.Index (page) where

import           Data.M0ORI

import           Html.Common.Head
import           Html.Common.Link

import           Text.Blaze.Html5            as H hiding (main)
import           Text.Blaze.Html5.Attributes as A

pageHamRadio :: Html
pageHamRadio = li ! class_ "nav-item" $ do
    input ! type_ "radio" ! A.style "display:none" ! checked "checked" ! name "selected" ! A.id "Ham Radio" ! value "Ham Radio"
    (H.label ! class_ "mb-0" ! for "Ham Radio") . (a ! class_ "nav-link btn btn-sm") $ "Ham Radio"
    H.div ! class_ "page" ! A.id "ham" $ do
        (H.div ! class_ "row") . (H.div ! class_ "col my-md-3") $ small "» Ham Radio"
        (H.div ! class_ "row") . (H.div ! class_ "col-md-8 offset-md-2 py-3 bg-light") $ (do
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
            p $ extLink "https://www.qrzcq.com/call/M0ORI" "My QRZCQ page")

pageContact :: Html
pageContact = li ! class_ "nav-item" $ do
    input ! type_ "radio" ! A.style "display:none" ! name "selected" ! A.id "Contact" ! value "Contact"
    (H.label ! class_ "mb-0" ! for "Contact") . (a ! class_ "nav-link btn btn-sm") $ "Contact"
    H.div ! class_ "page" ! A.id "contact" $ do
        (H.div ! class_ "row") . (H.div ! class_ "col my-md-3") $ small "» Contact"
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

htmlHeader :: Html
htmlHeader = nav ! class_ "p-0 p-sm-2 navbar d-block d-sm-flex navbar-expand navbar-dark bg-primary" $ do
    a ! class_ "w-25 p-0 pt-1 pt-sm-0 w-sm-auto text-center text-sm-left navbar-brand" ! href "#intro" $ do
        img ! src "/img/favicon.png" ! A.style "height:32px" ! alt ""
        H.span ! class_ "title ml-2" $ "M0ORI: Dan Dart"
    H.div . (ul ! class_ "navbar-nav px-3") $ (do
            pageHamRadio
            pageContact)

page :: Html
page = docTypeHtml ! lang "en-GB" $ do
    htmlHead descTitle keywords mempty
    htmlHeader