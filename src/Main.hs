{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE UnicodeSyntax     #-}

module Main where

import           Blog.Feed
import           Control.Concurrent
import           Control.Monad.IO.Class
import           Control.Monad.Trans.Reader
import           Data.ByteString                (isPrefixOf)
import           Data.Env
import           Data.Map                       ((!))
import           Data.Maybe
import           Data.Text                      (Text)
import qualified Data.Text                      as T
import           Data.Text.IO                   as TIO
import           Html.Common.Markdowns
import           Network.Wai
import           Network.Wai.Application.Static
import           Network.Wai.Handler.Warp
import           Network.Wai.Handler.WebSockets
import           Network.Wai.Middleware.Vhost
import           Network.WebSockets
import           Prelude                        hiding (putStrLn)
import           System.Environment             (lookupEnv)
import           System.FilePath
import           Util.Build
import           WaiAppStatic.Types

sites ∷ [Text]
sites = [
    "madhacker",
    "blog",
    "dandart",
    "jolharg",
    "m0ori"
    ]

build ∷ WebsitesIO ()
build = do
    websites <- ask
    mapM_ buildSite websites
    liftIO $ do
        runReaderT buildBlog $ websites ! "blog"
        runReaderT buildSite $ websites ! "dandart"
        runReaderT buildJH $ websites ! "jolharg"
        runReaderT buildSite $ websites ! "m0ori"
        runReaderT buildMH $ websites ! "madhacker"

wsApp ∷ ServerApp
wsApp pending_conn = do
    _ <- acceptRequest pending_conn
    -- Just hang
    a <- newEmptyMVar
    readMVar a

serve ∷ IO ()
serve = do
    -- TODO find correct build
    runReaderT build development
    port <- fromMaybe "80" <$> lookupEnv "PORT"
    putStrLn "Serving all websites:"
    -- TODO move sites into WebsitesIO
    mapM_ (\host -> putStrLn $ "http://" <> T.pack host <> ".localhost:" <> T.pack port) sites

    runEnv 80 $ websocketsOr defaultConnectionOptions wsApp (vhost (fmap (\name -> (
            isPrefixOf name . fromMaybe "" . requestHeaderHost,
            staticApp $ (defaultWebAppSettings ".sites/" <> name <> "/"){ ssIndices = indices }
        -- TODO move sites into WebsitesIO
        )) sites) . staticApp $ defaultWebAppSettings ".sites/")
    where indices = mapMaybe toPiece ["index.html"]

main ∷ IO ()
main = runReaderT build production
