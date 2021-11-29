{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE UnicodeSyntax     #-}

module Html.DanDart.Page.Maths where

import           Data.Env.Types
import           Html.Common.Link
import           Html.Common.Page
import           Html.Common.Shortcuts
import           Text.Blaze.Html5            as H hiding (main)

pageMaths ∷ WebsiteM Html
pageMaths = makePage "maths" "Maths" defaultLayout notDefaultPage $ do
    p "Mathematics has always been a great pastime for me."
    p $ do
        "I have invented quite a few visualisations and generators for several interesting pieces of mathematics, some of which you can see and try on repos like my projects repo on GitHub ("
        extLink (projectsSource <> "/haskell/maths/src") "Haskell examples"
        ", "
        extLink (projectsSource <> "/js/maths") "JS examples"
        ")"
    p "Some web-based notable (read: working right now) examples are:"
    ul $ do
        li $ extLink (ghPagesProjects <> "js/maths/src/cfe/index.html") "Continued Fraction Expander"
        li $ do
            "A set of gravity simulators:"
            ul $ do
                li $ extLink (ghPages <> "gravity/2/gravity.html") "Sun version (left click for planets, middle click for stars)"
                li $ extLink (ghPages <> "gravity/4/gravity.html") "Black hole version"
        li $ extLink (ghPagesProjects <> "js/maths/src/cellautomata/") "Cell Automata"
        li $ do
            extLink (ghPagesProjects <> "js/maths/src/graphAndSound/") "Graph and Sound Demos"
            br
            "(one of these is actually the answer similar to "
            extLink (yt <> "zSL8asTgpzA") "the riddle"
            " I posed on my YouTube channel)"
        li $ extLink (ghPagesProjects <> "js/rolling/") "Rolling Shutter effect example"
        li $ extLink (ghPages <> "heartish/cardint.html") "Interactive Cardioid (keyboard only)"
        li $ extLink (ghPages <> "heartish/card.html") "Random Cardioid"
        li $ extLink (ghPages <> "heartish/index.html") "Circle Reflection"
    p $ do
        "My approved "
        extLinkTitle "https://oeis.org" "Online Encyclopedia of Integer Sequences" "OEIS"
        " sequences are:"
    ul $ do
        li $ extLink (oeis <> "275124") "A275124: Multiples of 5 where Pisano periods of Fibonacci numbers A001175 and Lucas numbers A106291 agree."
        li $ extLink (oeis <> "275167") "A275167: Pisano periods of A275124."
        li $ extLink (oeis <> "308267") "A308267: Numbers which divide their Zeckendorffian format exactly."
        li $ extLink (oeis <> "309979") "A309979: Hash Parker numbers: Integers whose real 32nd root's first six nonzero digits (after the decimal point) rearranged in ascending order are equal to 234477."