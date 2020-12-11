{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE UnicodeSyntax #-}

module Util.QueryString where

lookupQueryString ∷ QueryString → ByteString → ByteString
lookupQueryString qs key =
  (\[QPair _ (QValue (Just b))] -> b) . Prelude.filter (\(QPair a _) -> a == key) $
  (\(QList x) -> x) qs