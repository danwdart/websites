{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE UnicodeSyntax     #-}

module Html.DanDart.Page.Music where

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

pageMusic ∷ WebsiteM Html
pageMusic = makePage "music" "Music" defaultLayout notDefaultPage $ do
    p "I play the guitar, keyboard and synthesiser."
    p "I've created the following pieces of music/sound effects:"
    audioFile "Gothic Orchestra" "GothicOrchestra" "SatanicOrchestra"
    audioFile "Shall It Be" "ShallItBe" "ShallItBe"
    audioFile "Swim Deep (take 1)" "SwimDeepTake1" "SwimDeepTake1"