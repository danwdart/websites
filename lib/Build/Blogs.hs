{-# LANGUAGE OverloadedStrings #-}

module Build.Blogs where

import           Control.Monad.IO.Class
import           Control.Monad.Reader
import           Data.Env.Types
import qualified Data.Text              as T
import qualified Data.Text.IO           as TIO
import           Html.Common.Blog.Feed
import           Html.Common.Blog.Link
import           Make
import           System.FilePath
import           Text.Blaze.Html5 as H hiding (main, title)

build ∷ (Html → Html → WebsiteM Html) -> WebsiteM Html -> WebsiteIO ()
build page page404 = do
  url' <- asks url
  title' <- asks title
  slug' <- asks slug
  (sortedPosts, renderedPosts) <- buildMD ("posts" </> T.unpack slug') "post" (const mempty)
  liftIO . TIO.writeFile (".sites" </> T.unpack slug' </> "atom.xml") $ makeRSSFeed url' title' sortedPosts
  make slug' (page (makeLinks sortedPosts) renderedPosts) page404