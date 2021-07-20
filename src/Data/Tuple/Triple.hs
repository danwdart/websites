{-# LANGUAGE UnicodeSyntax #-}
module Data.Tuple.Triple (t1, t2) where

-- Because I can't be bothered with lenses for now.

t1 ∷ (a, b, c) → a
t1 (a, _, _) = a

t2 ∷ (a, b, c) → b
t2 (_, b, _) = b
