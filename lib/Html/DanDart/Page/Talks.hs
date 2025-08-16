{-# LANGUAGE OverloadedStrings #-}

module Html.DanDart.Page.Talks where

import Control.Monad.Reader
import Data.Env.Types
import Data.NonEmpty               qualified as NE
import Html.Common.Bootstrap
import Html.Common.Card
import Html.Common.Page
import Text.Blaze.Html5      as H hiding (main)

pageTalks ∷ (MonadReader Website m) ⇒ m Html
pageTalks = plainBreadcrumb (NE.trustedNonEmpty "Talks") . makePage "talks" "Talks" defaultLayout notDefaultPage $ do
    p "Here is a list of some of the talks I've done."
    row $ do
        card "img/talks/chaplin.png" "Another YouTube" "Why do we need another YouTube: the first open source, self-hosted video sharing website at OggCamp '13." "https://joind.in/event/oggcamp-13/why-do-we-need-another-youtube"
        card "img/talks/uefi.png" "UEFI: WTF?" "Explaining the ins and outs of UEFI at OggCamp '13." "https://joind.in/event/oggcamp-13/uefi-wtf"
        card "img/talks/bibud.png" "Bibud" "Web desktop for aggregating your life online. Last edited on 2012-12-20." ""
        card "img/talks/geekup-2012-08-10.png" "Linux news, catching up" "at GeekUp Manchester on August 10, 2012" ""
        card "img/talks/geekup-2012-05-11.png" "Linux releases, Raspberry Pi, etc." "at GeekUp Manchester on May 11th 2012" ""
        card "img/talks/sblug-2011-01.png" "Apps, rooting and ROMs" "at SBLUG in Jan '11." ""
        -- have "video" if exists
        -- have "slides" if exists
        
        
