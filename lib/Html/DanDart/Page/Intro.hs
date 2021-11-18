{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE UnicodeSyntax     #-}

module Html.DanDart.Page.Intro where

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

pageIntro ∷ WebsiteM Html
pageIntro = makePage "intro" "Intro" defaultLayout defaultPage $ do
    p "Hello, my name is Dan."
    p "I am a software engineer, mathematics lover, radio ham and musician."
    p $ do
        "I work remotely to care for my future wife, "
        extLink "https://yanderedarling.com/" "Toga Himiko"
        "."
    p "I also enjoy discordant and nonsensical commentary."
    p "I can speak about maths, physics, computer science and linguistics at length."
    p "You can find out more by using the links at the top."
    br
    p $ do
        extLink "https://html.spec.whatwg.org/" $ img ! A.style "height: 16px" !  src "https://upload.wikimedia.org/wikipedia/commons/a/a1/WHATWG_logo.svg"
        extLink "/humans.txt" $ img ! src "/img/humanstxt-isolated-blank.gif"