{-# LANGUAGE OverloadedStrings #-}

module Build.BlogJolHarg where

import           Control.Monad.IO.Class
import           Control.Monad.Reader
import           Data.Env.Types
import qualified Data.Text              as T
import qualified Data.Text.IO           as TIO
import           Html.Blog.Index
import           Html.Common.Blog.Feed
import           Html.Common.Blog.Link
import           Make
import           System.FilePath

build ∷ WebsiteIO ()
build = do
  url' <- asks url
  title' <- asks title
  slug' <- asks slug
  (Blog (PostsLocation postsDir')) <- asks siteType
  (sortedPosts, renderedPosts) <- buildMD postsDir' "post" (const mempty)
  liftIO . TIO.writeFile (".sites" </> T.unpack slug' </> "atom.xml") $ makeRSSFeed url' title' sortedPosts
  make slug' (page (makeLinks sortedPosts) renderedPosts) page404