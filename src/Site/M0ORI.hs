{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE UnicodeSyntax     #-}

module Site.M0ORI where

import           Build.Utils
import           Data.Maybe                     (fromMaybe, mapMaybe)
import           Html.M0ORI.Index
import           Network.Wai.Application.Static
import           Network.Wai.Handler.Warp
import           System.Environment             (lookupEnv)
import           WaiAppStatic.Types

build ∷ IO ()
build = make "m0ori" page page404

serve ∷ IO ()
serve = makeServe build "m0ori"