{-# LANGUAGE OverloadedStrings #-}

module Build.JolHarg where

import Configuration.Dotenv
import Control.Monad (void)
import Control.Monad.IO.Class
import Control.Monad.Reader
import Data.Env.Types
import Html.Common.GitHub
import Html.JolHarg.Index
import Make
import Network.HTTP.Req
import Text.Blaze.Html5       as H hiding (main)

build ∷ forall m. (MonadReader Website m, MonadIO m, MonadFail m) ⇒ m ()
build = do
  slug' <- asks slug
  void . liftIO $ loadFile defaultConfig
  [reposJH, reposDan] <- traverse (liftIO . runReq defaultHttpConfig . getRepos) ["jolharg", "danwdart"]
  let page' ∷ m Html
      page' = runReader page (reposJH <> reposDan)

  make slug' page' page404
