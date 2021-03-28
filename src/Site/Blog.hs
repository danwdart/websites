{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE UnicodeSyntax     #-}

module Site.Blog where

import           Blog.Feed             (makeRSSFeed)
import           Util.Build           (make, makeServe)
import qualified Data.Text.IO          as TIO
import           Html.Blog.Index  (page, page404)
import           Site.Markdowns

build ∷ Bool -> IO ()
build dev = do
  (sortedPosts, renderedPosts, renderedLinks) <- buildMD "posts" "post"
  TIO.writeFile ".sites/blog/atom.xml" $ makeRSSFeed "https://blog.dandart.co.uk" "Dan Dart's Blog" sortedPosts
  make "blog" (page dev renderedLinks renderedPosts) page404

serve ∷ Bool -> IO ()
serve dev = makeServe (build dev) "blog"
