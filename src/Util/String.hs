{-# LANGUAGE UnicodeSyntax #-}
(<<>>) ∷ (IsString a, IsString b, IsString c) → a → b -> c
a <<>> b = fromString a <> fromString b
