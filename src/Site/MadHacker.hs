{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE UnicodeSyntax     #-}

module Site.MadHacker where

import           Blog.Feed             (makeRSSFeed)
import           Blog.Link             (makeLinks)
import           Blog.Post             (makeBlogPost, renderPost)
import           Blog.Types            (BlogMetadata (date, draft),
                                        BlogPost (metadata))
import           Build.Utils           (make, makeServe)
import           Control.Monad         (filterM)
import           Data.List             (sortOn)
import           Data.Ord              (Down (Down))
import qualified Data.Text.IO          as TIO
import           Html.MadHacker.Index  (page, page404)
import           Html.MadHacker.Suffix (renderStars)
import           System.Directory      (doesFileExist, getDirectoryContents)
import           System.FilePath       ((</>))

build ∷ IO ()
build = do
  files <- getDirectoryContents "reviews"
  let fileNames = ("reviews/" </>) <$> files -- if used in same line, use Compose
  validFiles <- filterM doesFileExist fileNames
  reviews <- sequence $ makeBlogPost "reviews" <$> validFiles
  let sortedPosts = sortOn (Down . date . metadata) . filter (not . draft . metadata) $ reviews
  let renderedPosts = foldMap (renderPost "review" renderStars) sortedPosts
  TIO.writeFile ".sites/madhacker/atom.xml" $ makeRSSFeed "https://madhackerreviews.com" "Mad Hacker Tech Reviews" sortedPosts
  let renderedLinks = makeLinks sortedPosts
  make "madhacker" (page renderedLinks renderedPosts) page404

serve ∷ IO ()
serve = makeServe build "madhacker"
