{-# LANGUAGE OverloadedStrings #-}

module Html.DanDart.Page.Morality where

import Control.Monad.Reader
import Data.Env.Types
-- import Data.Site.DanDart
-- import Data.String
-- import Html.Common.Link
import Html.Common.Page
-- import Html.Common.Shortcuts
import Text.Blaze.Html5      as H hiding (main)

pageMorality ∷ (MonadReader Website m) ⇒ m Html
pageMorality = makePage "morality" "Morality" defaultLayout notDefaultPage $ do
    p "I have discovered that there are a few core tenets which govern proper behaviour:"
    ul .
        li $ do
            "Everybody must treat everybody, including themselves, equally respectfully and properly. Specifically:"
            ul $ do
                li "Treat everybody as somebody who is deserving of respect."
                li "Do not harm anybody."
                li "Do not allow anybody to come into harm."
                li "Make concessions to those less fortunate or able than you."