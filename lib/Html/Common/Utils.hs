module Html.Common.Utils where

import Data.List        qualified as L
import Text.Blaze.Html5 as H hiding (main)

intercalateAttr ∷ AttributeValue → [AttributeValue] → AttributeValue
intercalateAttr x = L.foldl1' (\acc y -> acc <> x <> y)
