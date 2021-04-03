{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE UnicodeSyntax     #-}

module Site.M0ORI where

import           Data.Env
import           Html.M0ORI.Index
import           Util.Build

build ∷ WebsiteIO ()
build = make "m0ori" page page404

serve ∷ WebsiteIO ()
serve = makeServe build "m0ori"
