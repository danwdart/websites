module Build.DanDart where

import Build.Normal       qualified as Normal
import Data.Env.Types
import Html.DanDart.Index

build ∷ WebsiteIO ()
build = Normal.build page page404
