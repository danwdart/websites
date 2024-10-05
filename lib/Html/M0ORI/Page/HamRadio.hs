{-# LANGUAGE OverloadedStrings #-}

module Html.M0ORI.Page.HamRadio where

import Control.Monad.Reader
import Data.Env.Types
import Data.Site.M0ORI
import Html.Common.Link
import Html.Common.Page
import Text.Blaze.Html5            as H hiding (main)
import Text.Blaze.Html5.Attributes as A

pageHamRadio ∷ MonadReader Website m ⇒ m Html
pageHamRadio = makePage "ham" "Ham Radio" defaultLayout defaultPage $ do
    p "I am a UK full-licenced radio amateur, and have been issued the callsign M0ORI."
    p $ do
        "My nearest radio club is Exmouth Amateur Radio Club, but they don't seem to have a website. As far as I have been made aware, they meet on the third Wednesday of the month in Marpool Hill Scout Headquarters at 19:30, although I personally cannot attend for mobility and caring reasons."
    p $ do
        "I work on a "
        extLink "https://www.baofengradio.co.uk/uv-5r-black-vhf-uhf/" "Baofeng UV-5R"
        " (8W FM VHF/UHF transceiver) and a "
        extLink "https://www.yaesu.com/indexVS.cfm?cmd=DisplayProducts&ProdCatID=102&encProdID=06014CD0AFA0702B25B12AB4DC9C0D27" "Yaesu FT-817"
        " (5W, all-mode HF, VHF, UHF transceiver)"
    p "You may sometimes find me on:"
    ul $ do
        li "FM/APRS/AX.25/FreeDV on VHF/UHF in East Devon, UK (IO80)."
        li "FT modes and occasionally PSK on HF"
    p $ extLink "https://www.qrzcq.com/call/M0ORI" "My QRZCQ page"
    br
    (H.div ! A.id "rigref-solar-widget")
        . (a ! href "https://www.hamqsl.com/solar.html" ! target "_blank")
        $ (img ! src (stringValue imgUrl))
