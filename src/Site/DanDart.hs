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
build = make "dandart" page

serve ∷ IO ()
serve = do
    putStrLn "Building..."
    build
    port <- fromMaybe "80" <$> lookupEnv "PORT"
    putStrLn $ "Serving on http://localhost:" <> port
    runEnv 80 . staticApp $ (defaultWebAppSettings ".sites/dandart/"){ ssIndices = mapMaybe toPiece ["index.html"] }
