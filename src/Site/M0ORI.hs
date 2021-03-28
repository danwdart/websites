{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE UnicodeSyntax     #-}

module Site.M0ORI where

import Control.Monad.Trans.Reader
import Data.Env
import           Util.Build
import           Html.M0ORI.Index

build ∷ WebsiteIO ()
build = do
  dev' <- asks dev
  make "m0ori" (page dev') page404

serve ∷ WebsiteIO ()
serve = makeServe build "m0ori"
