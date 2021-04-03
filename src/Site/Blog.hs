{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE UnicodeSyntax     #-}

module Site.Blog where

import           Blog.Feed                  (makeRSSFeed)
import           Control.Monad.IO.Class
import           Control.Monad.Trans.Reader
import           Data.Env
import qualified Data.Text.IO               as TIO
import           Html.Blog.Index            (page, page404)
import           Site.Markdowns
import           Util.Build                 (make, makeServe)

build ∷ WebsiteIO ()
build = do
  url' <- asks url
  title' <- asks title
  (sortedPosts, renderedPosts, renderedLinks) <- buildMD "posts" "post"
  liftIO . TIO.writeFile ".sites/blog/atom.xml" $ makeRSSFeed url' title' sortedPosts
  make "blog" (page renderedLinks renderedPosts) page404

serve ∷ WebsiteIO ()
serve = makeServe build "blog"
