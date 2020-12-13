{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE UnicodeSyntax     #-}

module Site.DanDart where

import           Build.Utils
import           Html.DanDart.Index

build ∷ IO ()
build = make "dandart" page page404

serve ∷ IO ()
serve = makeServe build "dandart"