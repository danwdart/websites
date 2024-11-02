
module Build.M0ORI where

import Build.Normal         qualified as Normal
import Control.Monad.Reader
import Data.Env.Types
import Html.M0ORI.Index

build ∷ (MonadReader Website m, MonadIO m) ⇒ m ()
build = Normal.build page page404
