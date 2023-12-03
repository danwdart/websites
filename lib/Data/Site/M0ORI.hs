{-# LANGUAGE OverloadedStrings #-}

module Data.Site.M0ORI where


import Text.Blaze.Html5 as H hiding (main)

keywords ∷ [AttributeValue]
keywords = [
    "dan",
    "dart",
    "dandart",
    "daniel dart",
    "dan dart",
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
    "haskell",
    "typescript",
    "react.js",
    "react",
    "radio",
    "call",
    "sign",
    "ham",
    "m0ori",
    "yaesu",
    "qrz"
    ]

title' ∷ String
title' = "M0ORI call sign: Dan Dart, England"

description' :: String
description' = "The M0ORI callsign is owned by Dan Dart located in England. He works on HF and VHF in Exmouth."

url' :: String
url' = "https://m0ori.com"

imgUrl :: String
imgUrl = "https://www.hamqsl.com/solar101vhf.php"