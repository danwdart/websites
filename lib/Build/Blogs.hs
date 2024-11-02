{-# LANGUAGE OverloadedStrings #-}

module Build.Blogs where

import Control.Monad.IO.Class
import Control.Monad.Reader
import Data.Env.Types
import Data.Text              qualified as T
import Data.Text.IO           qualified as TIO
import Html.Common.Blog.Feed
import Html.Common.Blog.Link
import Make
import System.FilePath
import Text.Blaze.Html5       as H hiding (main, title)

build ∷ (MonadReader Website m, MonadIO m) ⇒ (Html → Html → m Html) → m Html → m ()
build page page404 = do
  url' <- asks url
  title' <- asks title
  slug' <- asks slug
  email' <- asks email
  (sortedPosts, renderedPosts) <- buildMD ("posts" </> T.unpack slug') email' (const mempty)
  liftIO . TIO.writeFile (".sites" </> T.unpack slug' </> "atom.xml") $ makeRSSFeed url' title' sortedPosts
  make slug' (page (makeLinks sortedPosts) renderedPosts) page404
