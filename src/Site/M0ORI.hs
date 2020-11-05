{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE UnicodeSyntax     #-}

module Site.M0ORI where

import           Build.Utils
import           Data.Maybe
import           Html.M0ORI.Index
import           Network.Wai.Application.Static
import           Network.Wai.Handler.Warp
import           WaiAppStatic.Types

build ∷ IO ()
build = make "m0ori" page

serve ∷ IO ()
serve = do
    putStrLn "Building..."
    build
    putStrLn "Serving..."
    runEnv 80 . staticApp $ (defaultWebAppSettings ".sites/m0ori/"){ ssIndices = mapMaybe toPiece ["index.html"] }
