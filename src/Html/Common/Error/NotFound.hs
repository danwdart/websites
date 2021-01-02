{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE UnicodeSyntax     #-}

module Html.Common.Error.NotFound (defaultPage404) where

import           Html.Common.Bootstrap
import           Html.Common.Head
import           Text.Blaze.Html5            as H
import           Text.Blaze.Html5.Attributes as A

defaultPage404 ∷ String → [AttributeValue] → Html → Html
defaultPage404 descTitle keywords extraHead = docTypeHtml ! lang "en-GB" $ do
    htmlHead descTitle keywords extraHead
    body $ do
        row . (H.div ! class_ "col-12 text-center") $ do
            h1 "Oh no!"
            p "Sorry, I couldn't find that."
            p . (a ! href "/") $ "Back to main page"
