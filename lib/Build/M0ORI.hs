
module Build.M0ORI where

import qualified Build.Normal as Normal
import           Data.Env.Types
import           Html.M0ORI.Index

build ∷ WebsiteIO ()
build = Normal.build page page404