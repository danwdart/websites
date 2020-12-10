{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE UnicodeSyntax #-}

module Html.Common.Visit where

import Text.Blaze.Html5.Attributes as A
import Text.Blaze.Html5

visit :: AttributeValue -> Html
visit url = img ! src ("https://kkeacv0mpj.execute-api.eu-west-2.amazonaws.com/dev/visit.gif?url=" <> url) ! A.style "display: none"