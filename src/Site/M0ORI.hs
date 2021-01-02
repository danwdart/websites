{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE UnicodeSyntax     #-}

module Site.M0ORI where

import           Build.Utils
import           Html.M0ORI.Index

build ∷ IO ()
build = make "m0ori" page page404

serve ∷ IO ()
serve = makeServe build "m0ori"
