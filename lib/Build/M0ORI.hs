{-# LANGUAGE UnicodeSyntax #-}

module Build.M0ORI where

import           Control.Monad.Trans.Reader
import           Data.Env
import           Html.M0ORI.Index
import           Make

build ∷ WebsiteIO ()
build = do
    slug' <- asks slug
    make slug' page page404

serve ∷ WebsiteIO ()
serve = asks slug >>= makeServe build
