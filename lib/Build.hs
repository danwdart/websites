module Build where

import Control.Monad.IO.Class
import Control.Monad.Reader
import Data.Env               as Env
import Data.Env.Types         as Env
import Data.Foldable

build ∷ (MonadReader Env m, MonadIO m) ⇒ m ()
build = ask >>= traverse_ (\website -> liftIO $ runReaderT (Env.build website) website)

runBuild ∷ IO ()
runBuild = runReaderT Build.build production
