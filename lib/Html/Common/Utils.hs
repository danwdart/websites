{-# LANGUAGE UnicodeSyntax #-}
module Html.Common.Utils where

import           Text.Blaze.Html5 as H hiding (main)

intercalateAttr ∷ AttributeValue → [AttributeValue] → AttributeValue
intercalateAttr x = foldl1 (\acc y -> acc <> x <> y)
