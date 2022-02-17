{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE UnicodeSyntax     #-}

module Html.DanDart.Page.Music where

import           Data.Env.Types
import           Html.Common.Audio
import           Html.Common.Page
import           Text.Blaze.Html5  as H hiding (main)

pageMusic ∷ WebsiteM Html
pageMusic = makePage "music" "Music" defaultLayout notDefaultPage $ do
    p "I play the guitar, keyboard and synthesiser."
    p "I've created the following pieces of music/sound effects:"
    audioFile "Gothic Orchestra" "GothicOrchestra" "SatanicOrchestra"
    audioFile "Shall It Be" "ShallItBe" "ShallItBe"
    audioFile "Swim Deep (take 1)" "SwimDeepTake1" "SwimDeepTake1"
