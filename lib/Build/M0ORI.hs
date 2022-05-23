
module Build.M0ORI where

import           Control.Monad.Reader
import           Data.Env.Types
import           Html.M0ORI.Index
import           Make

build ∷ WebsiteIO ()
build = do
    slug' <- asks slug
    make slug' page page404

serve ∷ WebsiteIO ()
serve = asks slug >>= makeServe Build.M0ORI.build
