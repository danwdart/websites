module Build where

import Control.Exception
import Control.Exception.MissingAtomURIException
import Control.Lens
import Control.Monad.Error.Class
import Control.Monad.IO.Class
import Control.Monad.Reader
import Data.Env               as Env
import Data.Env.Types         as Env
import Data.Foldable

build ∷ (MonadReader Env m, MonadError MissingAtomURIException m, MonadIO m) ⇒ m ()
build = ask >>= traverse_ (\website -> runReaderT (website ^. Env.build) website)

runBuild ∷ IO ()
runBuild = runReaderT (modifyError throw Build.build) production
