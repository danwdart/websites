{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE UnicodeSyntax     #-}

module Data.M0ORI (keywords, descTitle) where

import           Html.Common.Shortcuts
import           Text.Blaze.Html5            as H hiding (main)
import           Text.Blaze.Html5.Attributes as A

keywords ∷ [AttributeValue]
keywords = [
    "dan",
    "dart",
    "radio",
    "call",
    "sign",
    "ham",
    "m0ori",
    "yaesu",
    "qrz"
    ]

descTitle ∷ String
descTitle = "M0ORI call sign: Dan Dart, England"
