{-# LANGUAGE UnicodeSyntax #-}
module Data.Text.Show where

import           Data.Text

tshow ∷ (Show a) ⇒ a → Text
tshow = pack . show
