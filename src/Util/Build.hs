{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE UnicodeSyntax     #-}

module Util.Build (mkdirp, make, makeServe) where

import           Control.Monad.IO.Class
import qualified Data.ByteString.Lazy.Char8     as BSL
import           Data.Env
import           Data.Maybe                     (fromMaybe, mapMaybe)
import           Data.Text                      (Text)
import qualified Data.Text                      as T
import           Data.Text.IO
import           Network.Wai.Application.Static
import           Network.Wai.Handler.Warp
import           Prelude                        hiding (putStrLn)
import           System.Directory
import           System.Environment             (lookupEnv)
import           System.FilePath
import           System.Path
import           Text.Blaze.Html.Renderer.Utf8
import           Text.Blaze.Html5
import           WaiAppStatic.Types

mkdirp ∷ String → IO ()
mkdirp = createDirectoryIfMissing True

make ∷ Text → WebsiteM Html → WebsiteM Html → WebsiteIO ()
make name page page404 = do
    let path = T.unpack name
    page' <- websiteMToWebsiteIO page
    page404' <- websiteMToWebsiteIO page404
    liftIO $ do
        copyDir "static/common" $ ".sites" </> path
        copyDir ("static" </> path) (".sites" </> path)
        BSL.writeFile (".sites" </> path </> "index.html") $ renderHtml page'
        BSL.writeFile (".sites" </> path </> "404.html") $ renderHtml page404'
        putStrLn $ name <> " compiled."

makeServe ∷ WebsiteIO () → Text → WebsiteIO ()
makeServe build slug' = do
    liftIO $ putStrLn "Building..."
    build
    liftIO $ do
        port <- fromMaybe "80" <$> lookupEnv "PORT"
        putStrLn $ "Serving on http://localhost:" <> T.pack port
        runEnv 80 . staticApp $ (defaultWebAppSettings $ ".sites/" <> T.unpack slug' <> "/"){ ssIndices = mapMaybe toPiece ["index.html"] }
