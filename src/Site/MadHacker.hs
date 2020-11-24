{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE UnicodeSyntax     #-}

module Site.MadHacker where

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
import           Html.MadHacker.Index                (page)
import           Html.MadHacker.Suffix          (renderStars)
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
  files <- getDirectoryContents "reviews"
  let fileNames = ("reviews/" </>) <$> files -- if used in same line, use Compose
  validFiles <- filterM doesFileExist fileNames
  reviews <- sequence $ makeBlogPost "reviews" <$> validFiles
  let sortedPosts = sortOn (Down . date . metadata) . filter (not . draft . metadata) $ reviews
  let renderedPosts = foldMap (renderPost "review" renderStars) sortedPosts
  TIO.writeFile ".sites/madhacker/atom.xml" $ makeRSSFeed "https://madhacker.dandart.co.uk" "Mad Hacker Tech Reviews" sortedPosts
  let renderedLinks = makeLinks sortedPosts
  make "madhacker" $ page renderedLinks renderedPosts

serve ∷ IO ()
serve = do
  putStrLn "Building..."
  build
  putStrLn "Serving..."
  runEnv 80 . staticApp $ (defaultWebAppSettings ".sites/madhacker/") {ssIndices = mapMaybe toPiece ["index.html"]}
