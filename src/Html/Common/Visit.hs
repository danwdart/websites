{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE UnicodeSyntax     #-}

module Html.Common.Visit where

import           Text.Blaze.Html5
import           Text.Blaze.Html5.Attributes as A

visit ∷ AttributeValue → Html
visit url = img ! src ("https://kkeacv0mpj.execute-api.eu-west-2.amazonaws.com/dev/v.gif?u=" <> url) ! A.style "display: none"
