{-# LANGUAGE OverloadedStrings #-}

module Build.MadHacker where

import           Control.Monad.IO.Class
import           Control.Monad.Reader
import           Data.Env.Types
import qualified Data.Text              as T
import qualified Data.Text.IO           as TIO
import           Html.Common.Blog.Feed
import           Html.Common.Blog.Link
import           Html.MadHacker.Index
import           Html.MadHacker.Suffix
import           Make
import           System.FilePath

build ∷ WebsiteIO ()
build = do
  slug' <- asks slug
  url' <- asks url
  title' <- asks title
  (sortedPosts, renderedPosts) <- buildMD "reviews" "review" renderStars
  liftIO . TIO.writeFile (".sites" </> T.unpack slug' </> "atom.xml") $ makeRSSFeed url' title' sortedPosts
  make slug' (page (makeLinks sortedPosts) renderedPosts) page404

serve ∷ WebsiteIO ()
serve = asks slug >>= makeServe Build.MadHacker.build
