{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE UnicodeSyntax     #-}

module Html.Common.Head where

import           Control.Monad.Reader
import           Data.Env.Types
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

htmlHead ∷ (MonadReader Website m) ⇒ String → [AttributeValue] → Html → m Html
htmlHead descTitle keywords extraHead = do
    livereload' <- asks livereload
    pure . H.head $ do
        H.title $ toHtml descTitle
        metas descTitle keywords
        commonCSS
        extraHead
        when livereload' . (script ! src "/js/livereload.js") $ mempty

