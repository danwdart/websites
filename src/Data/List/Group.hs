{-# LANGUAGE UnicodeSyntax #-}
module Data.List.Group where

import           Data.Function (on)
import           Data.List     (groupBy)

groupOn ∷ Eq a1 ⇒ (a2 → a1) → [a2] → [[a2]]
groupOn = groupBy . on (==)
