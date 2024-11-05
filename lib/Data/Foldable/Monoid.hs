module Data.Foldable.Monoid where

import Data.Foldable

foldA :: (Applicative f, Traversable t, Monoid a) => t (f a) -> f a
foldA = fmap fold . sequenceA