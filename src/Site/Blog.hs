{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE UnicodeSyntax     #-}

module Site.Blog where

import           Blog.Feed                      (makeRSSFeed)
import           Blog.Link                      (makeLinks)
import           Blog.Post                      (makeBlogPost, renderPost)
import           Blog.Types                     (BlogMetadata (date, draft),
                                                 BlogPost (metadata))
import           Build.Utils                    (make)
import           Control.Monad                  (filterM)
import           Data.List                      (sortOn)
import           Data.Maybe                     (mapMaybe)
import           Data.Ord                       (Down (Down))
import qualified Data.Text.IO                   as TIO
import           Html.Blog.Index                (page)
import           Network.Wai.Application.Static (defaultWebAppSettings,
                                                 staticApp)
import           Network.Wai.Handler.Warp       (runEnv)
import           System.Directory               (doesFileExist,
                                                 getDirectoryContents)
import           System.FilePath                ((</>))
import           WaiAppStatic.Types             (StaticSettings (ssIndices),
                                                 toPiece)

build ∷ IO ()
build = do
  let postsDir = "posts"
  files <- getDirectoryContents postsDir
  let fileNames = (postsDir </>) <$> files -- if used in same line, use Compose
  validFiles <- filterM doesFileExist fileNames
  posts <- sequence $ makeBlogPost postsDir <$> validFiles
  let sortedPosts = sortOn (Down . date . metadata) . filter (not . draft . metadata) $ posts
  let renderedPosts = foldMap (renderPost "post" (const mempty)) sortedPosts
  TIO.writeFile ".sites/blog/atom.xml" $ makeRSSFeed "https://blog.dandart.co.uk" "Dan Dart's Blog" sortedPosts
  let renderedLinks = makeLinks sortedPosts
  make "blog" $ page renderedLinks renderedPosts

serve ∷ IO ()
serve = do
  putStrLn "Building..."
  build
  putStrLn "Serving..."
  runEnv 80 . staticApp $ (defaultWebAppSettings ".sites/blog/") {ssIndices = mapMaybe toPiece ["index.html"]}
