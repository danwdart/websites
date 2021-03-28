{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE UnicodeSyntax     #-}

module Site.DanDart where

import Data.Env
import           Util.Build
import           Html.DanDart.Index

build ∷ WebsiteIO ()
build = make "dandart" page page404

serve ∷ WebsiteIO ()
serve = makeServe build "dandart"
