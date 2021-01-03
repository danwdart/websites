{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE UnicodeSyntax     #-}

module Html.Common.Visit where

import           Text.Blaze.Html5
import           Text.Blaze.Html5.Attributes as A

visitsUrl ∷ AttributeValue
visitsUrl = "https://kkeacv0mpj.execute-api.eu-west-2.amazonaws.com/dev/v.gif"

visit ∷ AttributeValue → Html
visit url = img ! src (visitsUrl <> "?u=" <> url) ! A.style "display: none"

visitPage ∷ AttributeValue → AttributeValue → Html
visitPage url page = img ! src (visitsUrl <> "?u=" <> url <> "&p=" <> page) ! A.style "width: 0; height: 0" ! customAttribute "loading" "lazy"

visitPageSub ∷ AttributeValue → AttributeValue → AttributeValue → Html
visitPageSub url page sub' = img ! src (visitsUrl <> "?u=" <> url <> "&p=" <> page <> "&s=" <> sub') ! A.style "width: 0; height: 0" ! customAttribute "loading" "lazy"
