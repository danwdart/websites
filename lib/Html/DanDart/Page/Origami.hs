{-# LANGUAGE OverloadedStrings #-}

module Html.DanDart.Page.Origami where

import Control.Monad.Reader
import Data.Env.Types
import Html.Common.Link
import Html.Common.Page
import Text.Blaze.Html5     as H hiding (main)

pageOrigami ∷ (MonadReader Website m) ⇒ m Html
pageOrigami = plainBreadcrumb "Origami" . makePage "origami" "Origami" defaultLayout notDefaultPage $ do
    p "I've been doing origami from a very young age. I will give some instructions on how to make some models that I've invented later on when I've figured out how to digitise them, but for now, I'll give you some of my favourite origami resources:"
    br
    p $ do
        extLink "https://amzn.to/2BWtHhY" "Complete Origami, a book by the late Eric Kenneway"
        br
        extLink "https://amzn.to/2BqLCO4" "Star Trek Paper Universe, a book by Andrew Pang"
        br
        extLink "https://amzn.to/2BspLWn" "Ultimate Origami Kit: The Complete Step-by-step Guide to the Art of Paper Folding, a book by John Morin"
        br
        extLink "https://amzn.to/2VzLzqe" "How to Make Origami Airplanes That Fly, a book by Gery Hsu"
