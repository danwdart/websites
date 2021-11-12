{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE UnicodeSyntax     #-}

module Html.DanDart.Page.Characters where

import           Data.Env
import           Data.Site.DanDart
import           Html.Common.Audio
import           Html.Common.Contact
import           Html.Common.Error.NotFound
import           Html.Common.Head
import           Html.Common.Header
import           Html.Common.Link
import           Html.Common.Page
import           Html.Common.Shortcuts
import           Html.Common.Social
import           Text.Blaze.Html5            as H hiding (main)
import           Text.Blaze.Html5.Attributes as A

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