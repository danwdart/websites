{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE UnicodeSyntax     #-}

module Site.M0ORI where

import           Build.Utils
import           Html.M0ORI.Index

build ∷ Bool -> IO ()
build dev = make "m0ori" (page dev) page404

serve ∷ Bool -> IO ()
serve dev = makeServe (build dev) "m0ori"
