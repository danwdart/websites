{-# LANGUAGE OverloadedStrings #-}

module Html.DanDart.Page.Films where

import Control.Monad.Reader
import Data.Env.Types
import Data.Foldable
import Data.NonEmpty               qualified as NE
import Data.String
import Html.Common.Link
import Html.Common.Page
import Html.Common.Shortcuts
import Html.DanDart.Data
import Text.Blaze.Html5      as H hiding (main)

pageFilms ∷ (MonadReader Website m) ⇒ m Html
pageFilms = plainBreadcrumb (NE.trustedNonEmpty "Films") . makePage "films" "Films" defaultLayout notDefaultPage $ do
    p "Here is a list of some of the films I've been involved with."
    row $ do
        cardLostMedia "Super Mario Bros vs War of the Worlds"
        card "What happens when you open the fridge?"
        cardLostMedia "Mock Tetley Tea Ads"
        cardLostMediaUnreally "Colin"
        card "The Making of Colin"
        

