module Build.Blog where

import Build.Blogs          qualified as Blogs
import Control.Exception.MissingAtomURIException
import Control.Monad.Error.Class
import Control.Monad.Reader
import Data.Env.Types
import Html.Blog.Index

build ∷ (MonadReader Website m, MonadError MissingAtomURIException m, MonadIO m) ⇒ m ()
build = Blogs.build page page404
