{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE UnicodeSyntax     #-}

module Site.DanDart where

import           Data.Env
import           Html.DanDart.Index
import           Util.Build

build ∷ WebsiteIO ()
build = make "dandart" page page404

serve ∷ WebsiteIO ()
serve = makeServe build "dandart"
