{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedLists   #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE UnicodeSyntax     #-}

module Serve where

import           Build
import           Control.Concurrent
import           Control.Monad.Trans.Reader
import           Data.Env
import           Data.Env.Types
import           Data.Maybe
import qualified Data.Set                       as S
import qualified Data.Text                      as T
import           Data.Text.Encoding
import           Data.Text.IO
import           Network.Wai
import           Network.Wai.Application.Static
import           Network.Wai.Handler.Warp
import           Network.Wai.Handler.WebSockets
import           Network.Wai.Middleware.Vhost
import           Network.WebSockets
import           Prelude                        hiding (putStrLn)
import           System.Environment             (lookupEnv)
import           WaiAppStatic.Types

serve ∷ IO ()
serve = do
    runReaderT Build.build development
    port <- fromMaybe "80" <$> lookupEnv "PORT"
    putStrLn "Serving all websites:"
    devSlugs <- runReaderT (asks (fmap slug . S.toList)) development
    mapM_ (\host -> putStrLn $ "http://" <> host <> ".localhost:" <> T.pack port) devSlugs
    runEnv 80 $ websocketsOr defaultConnectionOptions wsApp (
        vhost (
            fmap (
                \slug' -> (
                    T.isPrefixOf slug' .  decodeUtf8 . fromMaybe "" . requestHeaderHost,
                    staticApp $ (
                        defaultWebAppSettings (".sites/" <> T.unpack slug' <> "/")
                    ) { ssIndices = indices }
                )
            )
            devSlugs
        ) . staticApp $ defaultWebAppSettings ".sites/")
    where
        indices = mapMaybe toPiece ["index.html"]
        wsApp ∷ ServerApp
        wsApp pending_conn = do
            _ <- acceptRequest pending_conn
            -- Just hang
            a <- newEmptyMVar
            readMVar a
