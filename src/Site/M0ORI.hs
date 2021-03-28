{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE UnicodeSyntax     #-}

module Site.M0ORI where

import Data.Env
import           Util.Build
import           Html.M0ORI.Index

build ∷ WebsiteIO ()
build = make "m0ori" page page404

serve ∷ WebsiteIO ()
serve = makeServe build "m0ori"
