{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE UnicodeSyntax     #-}

module Site.Blog where

import           Blog.Feed             (makeRSSFeed)
import           Build.Utils           (make, makeServe)
import qualified Data.Text.IO          as TIO
import           Html.Blog.Index  (page, page404)
import           Site.Markdowns

build ∷ IO ()
build = do
  (sortedPosts, renderedPosts, renderedLinks) <- buildMD "posts" "post"
  TIO.writeFile ".sites/blog/atom.xml" $ makeRSSFeed "https://blog.dandart.co.uk" "Dan Dart's Blog" sortedPosts
  make "blog" (page renderedLinks renderedPosts) page404

serve ∷ IO ()
serve = makeServe build "blog"
