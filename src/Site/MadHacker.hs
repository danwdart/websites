{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE UnicodeSyntax     #-}

module Site.MadHacker where

import           Blog.Feed                  (makeRSSFeed)
import           Blog.Link                  (makeLinks)
import           Control.Monad.IO.Class
import           Control.Monad.Trans.Reader
import           Data.Env
import qualified Data.Text.IO               as TIO
import           Html.MadHacker.Index       (page, page404)
import           Html.MadHacker.Suffix
import           Site.Markdowns
import           Util.Build                 (make, makeServe)

build ∷ WebsiteIO ()
build = do
  url' <- asks url
  title' <- asks title
  (sortedPosts, renderedPosts) <- buildMD "reviews" "review" renderStars
  liftIO . TIO.writeFile ".sites/madhacker/atom.xml" $ makeRSSFeed url' title' sortedPosts
  make "madhacker" (page (makeLinks sortedPosts) renderedPosts) page404

serve ∷ WebsiteIO ()
serve = makeServe build "madhacker"
