module Build.MadHacker where

import qualified Build.Blogs as Blogs
import           Data.Env.Types
import           Html.MadHacker.Index

build ∷ WebsiteIO ()
build = Blogs.build page page404