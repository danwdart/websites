module Build.BlogJolHarg where

import qualified Build.Blogs as Blogs
import           Data.Env.Types
import           Html.BlogJolHarg.Index

build ∷ WebsiteIO ()
build = Blogs.build page page404