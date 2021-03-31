{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE UnicodeSyntax     #-}

module Site.MadHacker where

import Control.Monad.IO.Class
import Control.Monad.Trans.Reader
import Data.Env
import           Blog.Feed             (makeRSSFeed)
import           Util.Build            (make, makeServe)
import qualified Data.Text.IO          as TIO
import           Html.MadHacker.Index  (page, page404)
import           Site.Markdowns

build ∷ WebsiteIO ()
build = do
  url' <- asks url
  title' <- asks title
  (sortedPosts, renderedPosts, renderedLinks) <- buildMD "reviews" "review"
  liftIO . TIO.writeFile ".sites/madhacker/atom.xml" $ makeRSSFeed url' title' sortedPosts
  make "madhacker" (page renderedLinks renderedPosts) page404

serve ∷ WebsiteIO ()
serve = makeServe build "madhacker"
