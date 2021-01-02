{-# LANGUAGE UnicodeSyntax #-}

module Util.QueryString where

import           Data.ByteString.Char8  (ByteString)
import           Network.AWS.Data.Query

lookupQueryString ∷ QueryString → ByteString → ByteString
lookupQueryString qs key =
  (\[QPair _ (QValue (Just b))] -> b) . Prelude.filter (\(QPair a _) -> a == key) $
  (\(QList x) -> x) qs
