{-# LANGUAGE UnicodeSyntax #-}

module Site.DanDart where

import           Data.Env
import           Html.DanDart.Index
import           Util.Build

build ∷ WebsiteIO ()
build = do
    slug' <- asks slug
    make slug' page page404

serve ∷ WebsiteIO ()
serve = asks slug >>= makeServe build
