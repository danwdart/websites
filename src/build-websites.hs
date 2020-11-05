{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE UnicodeSyntax     #-}

module Main where

import           Data.ByteString                (isPrefixOf)
import           Data.Maybe

import           Network.Wai
import           Network.Wai.Application.Static
import           Network.Wai.Handler.Warp
import           Network.Wai.Middleware.Vhost
import           WaiAppStatic.Types

import qualified Site.Blog                      as B
import qualified Site.DanDart                   as D
import qualified Site.JolHarg                   as J
import qualified Site.M0ORI                     as M

build ∷ IO ()
build = do
    B.build
    D.build
    J.build
    M.build

serve ∷ IO ()
serve = do
    build
    putStrLn "Serving..."
    (runEnv 80 . vhost [
        (
            isPrefixOf "blog" . fromMaybe "" . requestHeaderHost,
            staticApp $ (defaultWebAppSettings ".sites/blog/"){ ssIndices = indices }
        ),
        (
            isPrefixOf "dandart" . fromMaybe "" . requestHeaderHost,
            staticApp $ (defaultWebAppSettings ".sites/dandart/"){ ssIndices = indices }
        ),
        (
            isPrefixOf "jolharg" . fromMaybe "" . requestHeaderHost,
            staticApp $ (defaultWebAppSettings ".sites/jolharg/"){ ssIndices = indices }
        ),
        (
            isPrefixOf "m0ori" . fromMaybe "" . requestHeaderHost,
            staticApp $ (defaultWebAppSettings ".sites/m0ori/"){ ssIndices = indices }
        )
        ]) . staticApp $ defaultWebAppSettings ".sites/"
    where indices = mapMaybe toPiece ["index.html"]

main ∷ IO ()
main = build
