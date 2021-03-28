{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE UnicodeSyntax     #-}

module Site.DanDart where

import Control.Monad.Trans.Reader
import Data.Env
import           Util.Build
import           Html.DanDart.Index

build ∷ WebsiteIO ()
build = do
  dev' <- asks dev
  make "dandart" (page dev') page404

serve ∷ WebsiteIO ()
serve = makeServe build "dandart"
