module Build.BlogJolHarg where

import Build.Blogs            qualified as Blogs
import Control.Monad.Reader
import Data.Env.Types
import Html.BlogJolHarg.Index

build ∷ (MonadReader Website m, MonadIO m) ⇒ m ()
build = Blogs.build page page404
