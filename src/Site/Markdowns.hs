{-# LANGUAGE UnicodeSyntax #-}

module Site.Markdowns (buildMD) where

import           Blog.Link        (makeLinks)
import           Blog.Post        (makeBlogPost, renderPost)
import           Blog.Types       (BlogMetadata (date, draft),
                                   BlogPost (metadata))
import           Control.Monad    (filterM)
import Control.Monad.IO.Class
import Data.Env
import           Data.List        (sortOn)
import           Data.Ord         (Down (Down))
import           Data.Text        (Text)
import           System.Directory (doesFileExist, getDirectoryContents)
import           System.FilePath  ((</>))
import           Text.Blaze.Html5 as H

buildMD ∷ FilePath → Text → WebsiteIO ([BlogPost], Html, Html)
buildMD postsDir postType = do
  files <- liftIO $ getDirectoryContents postsDir
  let fileNames = (postsDir </>) <$> files -- if used in same line, use Compose
  validFiles <- liftIO $ filterM doesFileExist fileNames
  posts <- liftIO . sequence $ makeBlogPost postsDir <$> validFiles

  let sortedPosts = sortOn (Down . date . metadata) . filter (not . draft . metadata) $ posts
  renderedPosts <- websiteMToWebsiteIO $ foldMap (renderPost postType (const mempty)) sortedPosts
  let renderedLinks = makeLinks sortedPosts

  pure (sortedPosts, renderedPosts, renderedLinks)
