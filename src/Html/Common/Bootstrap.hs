{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE UnicodeSyntax     #-}

module Html.Common.Bootstrap where

import           Text.Blaze.Html5            as H
import           Text.Blaze.Html5.Attributes as A

row ∷ Html → Html
row = H.div ! class_ "row"
