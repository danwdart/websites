{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE UnicodeSyntax     #-}

module Html.DanDart.Page.Favourites where

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

pageFavourites ∷ WebsiteM Html
pageFavourites = makePage "favourites" "Favourites" defaultLayout notDefaultPage $ do
    p "Here is a list of some of my favourite things."
    p $ strong "YouTube channels"
    ul $ do
        li $ strong "Maths"
        ul $ do
            li $ extLink (ytUser <> "numberphile") "Numberphile"
            li $ extLink (ytChan <> "UC1_uAIS3r8Vu6JjXWvastJg") "Mathologer"
        li $ strong "Science"
        ul . li $ extLink (ytUser <> "Vsauce") "Vsauce"
        li $ strong "Computer Science"
        ul . li $ extLink (ytUser <> "Computerphile") "Computerphile"
        li $ strong "General Knowledge / Other"
        ul . li $ extLink (ytChan <> "UC9pgQfOXRsp4UKrI8q0zjXQ") "Lindybeige"
    p $ do
        strong "TV shows/movies"
        " (I also have an "
        extLink "https://www.imdb.com/list/ls029966237/" "IMDB"
        " watchlist)"
    ul $ do
        li $ do
            "The Hannibal film and TV series:"
            ul $ do
                li $ do
                    extLink (imdb <> "0091474") "Manhunter (1986)"
                    br
                    "(well, you know... it's got... Iron Butterfly? *shrug*)"
                li $ extLink (imdb <> "0102926") "The Silence of the Lambs (1991)"
                li $ extLink (imdb <> "0212985") "Hannibal (2001)"
                li $ extLink (imdb <> "0289765") "Red Dragon (2002)"
                li $ extLink (imdb <> "0367959") "Hannibal Rising (2007)"
                li $ extLink (imdb <> "2243973") "Hannibal (TV series, 2013-)"
        li $ do
            "Star Trek films and series:"
            ul $ do
                li $ extLink (imdb <> "0092007") "Star Trek IV: The Voyage Home (1986)"
                li $ extLink (imdb <> "0117731") "Star Trek: First Contact (1996)"
                li $ extLink (imdb <> "0092455") "Star Trek: The Next Generation (1987-1994)"
        li $ do
            extLink (imdb <> "0056751") "Doctor Who (1963-)"
            " (my favourite Doctor is Tom Baker)"
    p $ strong "Music"
    ul $ mapM_ (\(title', list') -> do
        title'
        ul $ mapM_ li list'
        ) musicList
    p $ strong "Musical styles"
    ul $ mapM_ li musicalStyles
    p $ strong "Games"
    ul $ do
        li $ extLink "http://www.idsoftware.com/en-gb" "Quake"
        li $ extLink "http://sauerbraten.org/" "Cube 2: Sauerbraten"
        li $ extLink (wikipedia "The_Elder_Scrolls_IV:_Oblivion") "The Elder Scrolls IV: Oblivion"
        li $ extLink "https://ddlc.moe/" "Doki Doki Literature Club"
        li $ do
            extLink "https://danganronpa.us/" "Danganronpa"
            " (no despair girls / hope side spoilers please!)"
    p $ strong "Coding language:"
    ul . li $ (do
        extLink "https://www.haskell.org/" "Haskell"
        " (it's epic and pure!)")
    p $ strong "Operating Systems"
    ul $ do
        li $ do
            extLink "http://www.gnu.org/gnu/why-gnu-linux.en.html" "GNU/Linux"
            ": "
            extLink "http://nixos.org" "NixOS"
        li $ do
            extLink (wikipedia "Blue_Screen_of_Death") "Windows"
            ": "
            extLink (wikipedia "Windows_98#Windows_98_Second_Edition") "98 SE"
        li $ do
            "All-time: "
            extLink "http://riscos.com/riscos/310/index.php" "RISC OS"