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
build = make "m0ori" page

serve ∷ IO ()
serve = do
    putStrLn "Building..."
    build
    port <- fromMaybe "80" <$> lookupEnv "PORT"
    putStrLn $ "Serving on http://localhost:" <> port
    runEnv 80 . staticApp $ (defaultWebAppSettings ".sites/m0ori/"){ ssIndices = mapMaybe toPiece ["index.html"] }
