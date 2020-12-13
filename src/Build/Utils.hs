{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE UnicodeSyntax     #-}

module Build.Utils (mkdirp, make, makeServe) where

import qualified Data.ByteString.Lazy.Char8     as BSL
import           Data.Maybe                     (fromMaybe, mapMaybe)
import           Network.Wai.Application.Static
import           Network.Wai.Handler.Warp
import           System.Directory
import           System.Environment             (lookupEnv)
import           System.Path                    as H hiding (main)
import           Text.Blaze.Html.Renderer.Utf8
import           Text.Blaze.Html5
import           WaiAppStatic.Types

mkdirp ∷ String → IO ()
mkdirp = createDirectoryIfMissing True

make ∷ String → Html → Html → IO ()
make name page page404 = do
    copyDir "static/common" $ ".sites/" <> name
    copyDir ("static/" <> name) (".sites/" <> name)
    BSL.writeFile (".sites/" <> (name <> "/index.html")) $ renderHtml page
    BSL.writeFile (".sites/" <> (name <> "/404.html")) $ renderHtml page404
    putStrLn $ name <> " compiled."

makeServe :: IO () -> FilePath -> IO ()
makeServe build dir = do
    putStrLn "Building..."
    build
    port <- fromMaybe "80" <$> lookupEnv "PORT"
    putStrLn $ "Serving on http://localhost:" <> port
    runEnv 80 . staticApp $ (defaultWebAppSettings $ ".sites/" <> dir <> "/"){ ssIndices = mapMaybe toPiece ["index.html"] }
