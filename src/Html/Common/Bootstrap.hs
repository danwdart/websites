{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE UnicodeSyntax     #-}

module Html.Common.Bootstrap where

import           Text.Blaze.Html5            as H
import           Text.Blaze.Html5.Attributes as A

divClass :: AttributeValue -> Html -> Html
divClass class' = H.div ! A.class_ class'

row ∷ Html → Html
row = divClass "row"

col ∷ Html → Html
col = divClass "col"

formGroup :: Html -> Html
formGroup = divClass "form-group"