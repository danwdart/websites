{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE UnicodeSyntax     #-}

module Build where

import           Control.Monad
import           Control.Monad.IO.Class
import           Control.Monad.Reader
import           Data.Env                   as Env
import           Data.Env.Types             as Env
import           Prelude

build ∷ (MonadReader Env m, MonadIO m) => m ()
build = ask >>= mapM_ (\website -> liftIO $ runReaderT (Env.build website) website)

runBuild ∷ IO ()
runBuild = runReaderT Build.build production
