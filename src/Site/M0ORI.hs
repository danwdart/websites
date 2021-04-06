{-# LANGUAGE UnicodeSyntax     #-}

module Site.M0ORI where

import           Data.Env
import           Html.M0ORI.Index
import           Util.Build

build ∷ WebsiteIO ()
build = do
    slug' <- asks slug
    make slug' page page404

serve ∷ WebsiteIO ()
serve = do
    slug' <- asks slug
    makeServe build slug'
