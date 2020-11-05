{-# LANGUAGE OverloadedStrings #-}

module Site.Blog where

import           Blog.Feed
import           Blog.Link
import           Blog.Post
import           Blog.Types
import           Build.Utils
import           Control.Monad
import           Data.List
import           Data.Maybe
import           Data.Ord
import qualified Data.Text.IO                   as TIO
import           Html.Blog.Index
import           Network.Wai.Application.Static
import           Network.Wai.Handler.Warp
import           System.Directory
import           System.FilePath
import           WaiAppStatic.Types

build :: IO ()
build = do
  files <- getDirectoryContents "posts"
  let fileNames = ("posts/" </>) <$> files -- if used in same line, use Compose
  validFiles <- filterM doesFileExist fileNames
  posts <- sequence $ makeBlogPost <$> validFiles
  let sortedPosts = sortOn (Down . date . metadata) . filter (not . draft . metadata) $ posts
  let renderedPosts = foldMap renderPost sortedPosts
  TIO.writeFile ".sites/blog/atom.xml" $ makeRSSFeed sortedPosts
  let renderedLinks = makeLinks sortedPosts
  make "blog" $ page renderedLinks renderedPosts

serve :: IO ()
serve = do
  putStrLn "Building..."
  build
  putStrLn "Serving..."
  runEnv 80 . staticApp $ (defaultWebAppSettings ".sites/blog/") {ssIndices = mapMaybe toPiece ["index.html"]}
