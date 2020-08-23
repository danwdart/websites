{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE UnicodeSyntax     #-}

module Data.Blog (keywords, descTitle) where

import           Html.Common.Shortcuts
import           Text.Blaze.Html5            as H hiding (main)
import           Text.Blaze.Html5.Attributes as A

keywords ∷ [AttributeValue]
keywords = [
    "dan",
    "dart",
    "blog",
    "software",
    "engineer",
    "mathematics",
    "lover",
    "radio",
    "ham",
    "php",
    "javascript",
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
descTitle = "Dan Dart's Blog: Software Engineer, Mathematics Lover, Radio Ham, Musician"
