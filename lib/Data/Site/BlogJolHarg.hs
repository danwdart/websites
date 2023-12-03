{-# LANGUAGE OverloadedStrings #-}

module Data.Site.BlogJolHarg where

import Text.Blaze.Html5 as H hiding (main)

keywords ∷ [AttributeValue]
keywords = [
    "jolharg",
    "blog",
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

title' ∷ String
title' = "JolHarg Blog: Software Engineering Blog"

description' :: String
description' = "JolHarg's blog covers various pieces of technology, code and tutorials to help make your life easier."

url' :: String
url' = "https://blog.jolharg.com"

imgUrl :: String
imgUrl = "https://jolharg.com/img/header.png"