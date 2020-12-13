{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE UnicodeSyntax     #-}

module Site.DanDart where

import           Build.Utils
import           Data.Maybe                     (fromMaybe, mapMaybe)
import           Html.DanDart.Index
import           Network.Wai.Application.Static
import           Network.Wai.Handler.Warp
import           System.Environment             (lookupEnv)
import           WaiAppStatic.Types

build ∷ IO ()
build = make "dandart" page page404

serve ∷ IO ()
serve = makeServe build "dandart"