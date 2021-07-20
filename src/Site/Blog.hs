{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE UnicodeSyntax     #-}

module Site.Blog where

import           Blog.Feed                  (makeRSSFeed)
import           Blog.Link                  (makeLinks)
import           Control.Monad.IO.Class
import           Control.Monad.Trans.Reader
import           Data.Env
import qualified Data.Text                  as T
import qualified Data.Text.IO               as TIO
import           Html.Blog.Index            (page, page404)
import           Site.Markdowns
import           System.FilePath
import           Util.Build                 (make, makeServe)

build ∷ WebsiteIO ()
build = do
  url' <- asks url
  title' <- asks title
  slug' <- asks slug
  (sortedPosts, renderedPosts) <- buildMD "posts" "post" (const mempty)
  liftIO . TIO.writeFile (".sites" </> T.unpack slug' </> "atom.xml") $ makeRSSFeed url' title' sortedPosts
  make slug' (page (makeLinks sortedPosts) renderedPosts) page404

serve ∷ WebsiteIO ()
serve = asks slug >>= makeServe build
