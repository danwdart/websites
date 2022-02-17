{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE UnicodeSyntax     #-}

module Html.DanDart.Page.Characters where

import           Data.Env.Types
import           Data.Site.DanDart
import           Html.Common.Link
import           Html.Common.Page
import           Text.Blaze.Html5  as H hiding (main)

pageCharacters ∷ WebsiteM Html
pageCharacters = makePage "characters" "Characters" defaultLayout notDefaultPage $ do
    p "Some of my favourite characters and characters that I identify with are:"
    ul $ mapM_ (\(fandom', fandomLink, characters) -> do
        "from "
        extLink fandomLink fandom'
        ":"
        ul $ mapM_ (\(character, charLink, reason) -> li $ do
                extLink charLink character
                ", because "
                reason
            ) characters
        ) favCharacters
