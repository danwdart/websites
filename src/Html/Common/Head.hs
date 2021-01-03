{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE UnicodeSyntax     #-}

module Html.Common.Head (htmlHead) where

import Control.Monad (when)
import           Data.String

import           Html.Common.CSS
import           Html.Common.Utils

import           Text.Blaze.Html5            as H hiding (main)
import           Text.Blaze.Html5.Attributes as A

metas ∷ String → [AttributeValue] → Html
metas descTitle keywords = do
    meta ! charset "utf-8"
    mapM_ (\(aName, aCont) -> meta ! name aName ! content aCont) [
        ("description", fromString descTitle),
        ("keywords", intercalateAttr "," keywords)
        ]
    meta ! name "viewport" ! content "width=device-width, initial-scale=1"
    mapM_ (\(aHE, aCont) -> meta ! httpEquiv aHE ! content aCont) [
        ("Content-Type", "text/html; charset=utf-8"),
        ("Who-is-awesome", "Kaychan"),
        ("X-UA-Compatible", "IE=edge,chrome=1")
        ]

htmlHead ∷ Bool → String → [AttributeValue] → Html → Html
htmlHead dev descTitle keywords extraHead = H.head $ do
    H.title $ toHtml descTitle
    metas descTitle keywords
    commonCSS
    extraHead
    when dev . (script ! src "/js/livereload.js") $ mempty

