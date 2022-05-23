{-# LANGUAGE OverloadedStrings #-}

module Html.M0ORI.Page.HamRadio where

import           Control.Monad.Reader
import           Data.Env.Types
import           Html.Common.Link
import           Html.Common.Page
import           Text.Blaze.Html5            as H hiding (main)
import           Text.Blaze.Html5.Attributes as A

pageHamRadio ∷ MonadReader Website m ⇒ m Html
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
