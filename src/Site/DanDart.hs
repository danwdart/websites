{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE UnicodeSyntax     #-}

module Site.DanDart where

import           Util.Build
import           Html.DanDart.Index

build ∷ Bool -> IO ()
build dev = make "dandart" (page dev) page404

serve ∷ Bool -> IO ()
serve dev = makeServe (build dev) "dandart"
