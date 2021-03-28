{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE UnicodeSyntax     #-}

module Site.Blog where

import           Blog.Feed             (makeRSSFeed)
import Control.Monad.IO.Class
import Control.Monad.Trans.Reader
import Data.Env
import           Util.Build            (make, makeServe)
import qualified Data.Text.IO          as TIO
import           Html.Blog.Index  (page, page404)
import           Site.Markdowns

build ∷ WebsiteIO ()
build = do
  dev' <- asks dev
  (sortedPosts, renderedPosts, renderedLinks) <- liftIO $ buildMD "posts" "post"
  liftIO $ TIO.writeFile ".sites/blog/atom.xml" $ makeRSSFeed "https://blog.dandart.co.uk" "Dan Dart's Blog" sortedPosts
  make "blog" (page dev' renderedLinks renderedPosts) page404

serve ∷ WebsiteIO ()
serve = makeServe build "blog"
