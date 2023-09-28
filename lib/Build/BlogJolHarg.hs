module Build.BlogJolHarg where

import Build.Blogs            qualified as Blogs
import Data.Env.Types
import Html.BlogJolHarg.Index

build ∷ WebsiteIO ()
build = Blogs.build page page404
