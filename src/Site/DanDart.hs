{-# LANGUAGE OverloadedStrings #-}

module Site.DanDart where

import           Build.Utils
import           Data.Maybe
import           Html.DanDart.Index
import           Network.Wai.Application.Static
import           Network.Wai.Handler.Warp
import           WaiAppStatic.Types

build :: IO ()
build = make "dandart" page

serve :: IO ()
serve = do
    putStrLn "Building..."
    build
    putStrLn "Serving..."
    runEnv 80 . staticApp $ (defaultWebAppSettings ".sites/dandart/"){ ssIndices = mapMaybe toPiece ["index.html"] }
