module Build.Blog where

import qualified Build.Blogs as Blogs
import           Data.Env.Types
import           Html.Blog.Index

build ∷ WebsiteIO ()
build = Blogs.build page page404