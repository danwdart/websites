{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE UnicodeSyntax     #-}

module Data.Site.Blog (keywords, descTitle) where

import           Text.Blaze.Html5 as H hiding (main)

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
