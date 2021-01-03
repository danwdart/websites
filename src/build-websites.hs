{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE UnicodeSyntax     #-}

module Main where

import           Data.ByteString                (isPrefixOf)
import           Data.Maybe
import Data.Text (Text)
import           Network.Wai
import           Network.Wai.Application.Static
import           Network.Wai.Handler.Warp
import           Network.Wai.Handler.WebSockets
import           Network.Wai.Middleware.Vhost
import Network.WebSockets
import           WaiAppStatic.Types

import qualified Site.Blog                      as B
import qualified Site.DanDart                   as D
import qualified Site.JolHarg                   as J
import qualified Site.M0ORI                     as M
import qualified Site.MadHacker                 as R
import           System.Environment             (lookupEnv)
import Control.Concurrent

build ∷ Bool -> IO ()
build dev = do
    B.build dev
    D.build dev
    J.build dev
    M.build dev
    R.build dev

wsApp ∷ ServerApp
wsApp pending_conn = do
    conn <- acceptRequest pending_conn
    -- Just hang
    a <- newEmptyMVar
    readMVar a

serve ∷ IO ()
serve = do
    build True
    port <- fromMaybe "80" <$> lookupEnv "PORT"
    putStrLn "Serving all websites:"
    mapM_ (\host -> putStrLn $ "http://" <> host <> ".localhost:" <> port) [
        "madhacker",
        "blog",
        "dandart",
        "jolharg",
        "m0ori"
        ]
    runEnv 80 $ websocketsOr defaultConnectionOptions wsApp (vhost [
        (
            isPrefixOf "madhacker" . fromMaybe "" . requestHeaderHost,
            staticApp $ (defaultWebAppSettings ".sites/madhacker/"){ ssIndices = indices }
        ),
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
        ] . staticApp $ defaultWebAppSettings ".sites/")
    where indices = mapMaybe toPiece ["index.html"]

main ∷ IO ()
main = build False
