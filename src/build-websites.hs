{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE UnicodeSyntax     #-}

module Main where

import           Control.Monad.IO.Class
import           Control.Monad.Trans.Reader
import           Data.ByteString                (isPrefixOf)
import           Data.Env
import           Data.Map                       ((!))
import           Data.Maybe
import           Network.Wai
import           Network.Wai.Application.Static
import           Network.Wai.Handler.Warp
import           Network.Wai.Handler.WebSockets
import           Network.Wai.Middleware.Vhost
import           Network.WebSockets
import           WaiAppStatic.Types

import           Control.Concurrent
import qualified Site.Blog                      as B
import qualified Site.DanDart                   as D
import qualified Site.JolHarg                   as J
import qualified Site.M0ORI                     as M
import qualified Site.MadHacker                 as R
import           System.Environment             (lookupEnv)

build ∷ WebsitesIO ()
build = do
    websites <- ask
    liftIO $ do
        runReaderT B.build $ websites ! "blog"
        runReaderT D.build $ websites ! "dandart"
        runReaderT J.build $ websites ! "jolharg"
        runReaderT M.build $ websites ! "m0ori"
        runReaderT R.build $ websites ! "madhacker"

wsApp ∷ ServerApp
wsApp pending_conn = do
    _ <- acceptRequest pending_conn
    -- Just hang
    a <- newEmptyMVar
    readMVar a

serve ∷ IO ()
serve = do
    runReaderT build development
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
main = runReaderT build production
