{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE UnicodeSyntax     #-}

module Data.JolHarg (keywords, descTitle) where

import           Html.Common.Shortcuts
import           Text.Blaze.Html5            as H hiding (main)
import           Text.Blaze.Html5.Attributes as A

keywords ∷ [AttributeValue]
keywords = [
    "jolharg",
    "software",
    "dan",
    "dart",
    "software",
    "engineer",
    "mathematics",
    "haskell",
    "php",
    "javascript",
    "react",
    "react.js",
    "hoogle",
    "help",
    "computing",
    "computer",
    "serverless",
    "npm",
    "hask",
    "ask",
    "question",
    "css",
    "coffee",
    "coffeescript",
    "laravel",
    "zend",
    "framework",
    "linux",
    "gnu",
    "express.js",
    "ubuntu",
    "debian"
    ]

descTitle ∷ String
descTitle = "JolHarg: Your Software Engineering Partner"
