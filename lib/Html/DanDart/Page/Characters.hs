{-# LANGUAGE OverloadedStrings #-}

module Html.DanDart.Page.Characters where

import Control.Monad.Reader
import Data.Env.Types
import Data.Foldable
import Html.Common.Link
import Html.Common.Page
import Html.DanDart.Data
import Text.Blaze.Html5     as H hiding (main)

pageCharacters ∷ (MonadReader Website m) ⇒ m Html
pageCharacters = plainBreadcrumb "Characters" . makePage "characters" "Characters" defaultLayout notDefaultPage $ do
    p "Some of my favourite characters and characters that I identify with are:"
    ul $ traverse_ (\(fandom', fandomLink, characters) -> do
        "from "
        extLink fandomLink fandom'
        ":"
        ul $ traverse_ (\(character, charLink, reason) -> li $ do
                extLink charLink character
                ", because "
                reason
            ) characters
        ) favCharacters
