{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE UnicodeSyntax     #-}

module Main where

import           Control.Concurrent
import           Control.Monad.Trans.Reader
import           Data.ByteString                (isPrefixOf)
import           Data.Env
import           Data.Maybe
import qualified Data.Text                      as T
import           Data.Text.IO
import           Network.Wai
import           Network.Wai.Application.Static
import           Network.Wai.Handler.Warp
import           Network.Wai.Handler.WebSockets
import           Network.Wai.Middleware.Vhost
import           Network.WebSockets
import           Prelude                        hiding (putStrLn)
import           Site.Build
import           System.Environment             (lookupEnv)
import           WaiAppStatic.Types

wsApp ∷ ServerApp
wsApp pending_conn = do
    _ <- acceptRequest pending_conn
    -- Just hang
    a <- newEmptyMVar
    readMVar a

main ∷ IO ()
main = do
    runReaderT build development
    port <- fromMaybe "80" <$> lookupEnv "PORT"
    putStrLn "Serving all websites:"
    mapM_ (\host -> putStrLn $ "http://" <> T.pack host <> ".localhost:" <> T.pack port) [
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