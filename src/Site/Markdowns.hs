{-# LANGUAGE UnicodeSyntax #-}

module Site.Markdowns (buildMD) where

import           Blog.Link        (makeLinks)
import           Blog.Post        (makeBlogPost, renderPost)
import           Blog.Types       (BlogMetadata (date, draft),
                                   BlogPost (metadata))
import           Control.Monad    (filterM)
import           Data.List        (sortOn)
import           Data.Ord         (Down (Down))
import           Data.Text        (Text)
import qualified Data.Text.IO     as TIO
import           System.Directory (doesFileExist, getDirectoryContents)
import           System.FilePath  ((</>))
import           Text.Blaze.Html5 as H

buildMD ∷ FilePath → Text → IO ([BlogPost], Html, Html)
buildMD postsDir postType = do
  files <- getDirectoryContents postsDir
  let fileNames = (postsDir </>) <$> files -- if used in same line, use Compose
  validFiles <- filterM doesFileExist fileNames
  posts <- sequence $ makeBlogPost postsDir <$> validFiles

  let sortedPosts = sortOn (Down . date . metadata) . filter (not . draft . metadata) $ posts
  let renderedPosts = foldMap (renderPost postType (const mempty)) sortedPosts
  let renderedLinks = makeLinks sortedPosts

  pure (sortedPosts, renderedPosts, renderedLinks)
