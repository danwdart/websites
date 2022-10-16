module Build.DanDart where

import qualified Build.Normal       as Normal
import           Data.Env.Types
import           Html.DanDart.Index

build ∷ WebsiteIO ()
build = Normal.build page page404
