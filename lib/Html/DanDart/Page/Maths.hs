{-# LANGUAGE OverloadedStrings #-}

module Html.DanDart.Page.Maths where

import           Control.Monad.Reader
import           Data.Env.Types
import           Html.Common.Link
import           Html.Common.Page
import           Html.Common.Shortcuts
import           Text.Blaze.Html5      as H hiding (main)

pageMaths ∷ (MonadReader Website m) ⇒ m Html
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
        li $ extLink (oeis <> "355467") "A355467: a(n) is the smallest number which is greater than n and has more prime factors (with multiplicity) than n."
    p $ do
        "I also discovered "
        extLink (oeis <> "332049") "A332049: a(n) = (1/2) * Sum_{d|n, d > 1} d * phi(d)"
        " seemingly first "
        extLink "https://github.com/danwdart/projects/commit/8691b3ebcd4560f1eeae9cefccc017becce29256#diff-d849c35e9758cd82b45e19d5c4d74ee08e32d8a0092476e3189bebeca7156ac5" "in 2019"
        ", but wasn't the first to submit it."
    p $ do
        "I have "
        extLink "https://github.com/danwdart/projects/tree/master/haskell/maths/src/oeis" "a repository"
        " containing code that generates some more sequences in Haskell."
