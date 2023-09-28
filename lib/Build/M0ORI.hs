
module Build.M0ORI where

import Build.Normal     qualified as Normal
import Data.Env.Types
import Html.M0ORI.Index

build ∷ WebsiteIO ()
build = Normal.build page page404
