{-# LANGUAGE OverloadedStrings #-}

module Build.JolHarg where

import Build.Normal           qualified as Normal
import Configuration.Dotenv
import Control.Monad          (void)
import Control.Monad.IO.Class
import Control.Monad.Reader
import Data.Env.Types
import Data.Foldable
import Html.Common.GitHub
import Html.JolHarg.Index
import Network.HTTP.Req

build ∷ forall m. (MonadReader Website m, MonadIO m) ⇒ m ()
build = do
  void . liftIO $ loadFile defaultConfig
  -- foldTraverse? Monoid b => (a -> f b) -> t a -> f b = foldMapM/concatMapM
  results <- fold <$> traverse (runReq defaultHttpConfig . getRepos) ["jolharg", "danwdart"]
  Normal.build (runReader page results) page404
