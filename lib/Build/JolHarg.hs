{-# LANGUAGE OverloadedStrings #-}

module Build.JolHarg where

import Build.Normal           qualified as Normal
import Configuration.Dotenv
import Control.Monad          (void)
import Control.Monad.IO.Class
import Control.Monad.Reader
import Data.Env.Types
import Html.Common.GitHub
import Html.JolHarg.Index
import Network.HTTP.Req

build ∷ forall m. (MonadReader Website m, MonadIO m, MonadFail m) ⇒ m ()
build = do
  void . liftIO $ loadFile defaultConfig
  [reposJH, reposDan] <- traverse (liftIO . runReq defaultHttpConfig . getRepos) ["jolharg", "danwdart"]
  Normal.build (runReader page (reposJH <> reposDan)) page404
