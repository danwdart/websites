module Build.MadHacker where

import Build.Blogs          qualified as Blogs
import Data.Env.Types
import Html.MadHacker.Index

build ∷ WebsiteIO ()
build = Blogs.build page page404
