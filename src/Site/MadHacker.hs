{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE UnicodeSyntax     #-}

module Site.MadHacker where

import           Blog.Feed             (makeRSSFeed)
import           Build.Utils           (make, makeServe)
import qualified Data.Text.IO          as TIO
import           Html.MadHacker.Index  (page, page404)
import           Site.Markdowns

build ∷ IO ()
build = do
  (sortedPosts, renderedPosts, renderedLinks) <- buildMD "reviews" "review"
  TIO.writeFile ".sites/madhacker/atom.xml" $ makeRSSFeed "https://madhackerreviews.com" "Mad Hacker Tech Reviews" sortedPosts
  make "madhacker" (page renderedLinks renderedPosts) page404

serve ∷ IO ()
serve = makeServe build "madhacker"
