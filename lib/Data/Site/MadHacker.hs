{-# LANGUAGE OverloadedStrings #-}

module Data.Site.MadHacker where

import Text.Blaze.Html5 as H hiding (main)

keywords ∷ [AttributeValue]
keywords = [
    "exmouth",
    "exeter",
    "devon",
    "england",
    "united kingdom",
    "uk",
    "en_GB",
    "gb",
    "Great Britain",
    "Britain",
    "mad",
    "hacker",
    "tech",
    "technology",
    "reviews",
    "review",
    "dan",
    "dart",
    "dandart",
    "daniel dart",
    "dan dart",
    "haskell",
    "typescript",
    "react.js",
    "react"
    ]

title' ∷ String
title' = "The Mad Hacker: Tech Reviews by a crazy computer enthusiast"

description' ∷ String
description' = "Find tech and software reviews with a hackability twist, right here! Requests are accepted and review models are always non-sponsored."

url' ∷ String
url' = "https://madhackerreviews.com"

imgUrl ∷ String
imgUrl = "https://dandart.co.uk/img/header.png"
