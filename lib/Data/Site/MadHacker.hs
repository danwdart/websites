{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE UnicodeSyntax     #-}

module Data.Site.MadHacker (keywords, descTitle) where

import           Text.Blaze.Html5 as H hiding (main)

keywords ∷ [AttributeValue]
keywords = [
    "mad",
    "hacker",
    "tech",
    "technology",
    "reviews",
    "review",
    "dan",
    "dart"
    ]

descTitle ∷ String
descTitle = "The Mad Hacker: Tech Reviews by a crazy computer enthusiast"
