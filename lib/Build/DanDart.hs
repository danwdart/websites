{-# LANGUAGE UnicodeSyntax #-}

module Build.DanDart where

import           Control.Monad.Trans.Reader
import           Data.Env.Types
import           Html.DanDart.Index
import           Make

build ∷ WebsiteIO ()
build = do
    slug' <- asks slug
    make slug' page page404

serve ∷ WebsiteIO ()
serve = asks slug >>= makeServe Build.DanDart.build
