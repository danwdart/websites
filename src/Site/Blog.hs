{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE UnicodeSyntax     #-}

module Site.Blog where

import           Blog.Feed                      (makeRSSFeed)
import           Blog.Link                      (makeLinks)
import           Blog.Post                      (makeBlogPost, renderPost)
import           Blog.Types                     (BlogMetadata (date, draft),
                                                 BlogPost (metadata))
import           Build.Utils                    (make, makeServe)
import           Control.Monad                  (filterM)
import           Data.List                      (sortOn)
import           Data.Ord                       (Down (Down))
import qualified Data.Text.IO                   as TIO
import           Html.Blog.Index                (page404, page)
import           System.Directory               (doesFileExist,
                                                 getDirectoryContents)
import           System.FilePath                ((</>))

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
  make "blog" (page renderedLinks renderedPosts) page404

serve ∷ IO ()
serve = makeServe build "blog"