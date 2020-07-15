{-# LANGUAGE OverloadedStrings #-}
module Site.Blog where

import Build.Utils
import Cheapskate
import Control.Monad
import Data.Maybe
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Html.Blog.Index
import Network.Wai.Application.Static
import Network.Wai.Handler.Warp
import System.Directory
import System.FilePath
import Text.Blaze.Html5 as H hiding (main)
import Text.Blaze.Html5.Attributes as A
import WaiAppStatic.Types
import Debug.Trace

build :: IO ()
build = do
    files <- getDirectoryContents "posts"
    let fileNames = ("posts/" </>) <$> files -- if used in same line, use Compose
    validFiles <- filterM doesFileExist fileNames
    posts <- sequence $ TIO.readFile <$> validFiles
    make "blog" . page <$> toMarkup . markdown def $ T.concat posts

serve :: IO ()
serve = do
    putStrLn "Building..."
    build
    putStrLn "Serving..."
    runEnv 80 . staticApp $ (defaultWebAppSettings ".sites/blog/"){ ssIndices = mapMaybe toPiece ["index.html"] } 