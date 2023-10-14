{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Build.JolHarg where

import Configuration.Dotenv
import Control.Monad.IO.Class
import Control.Monad.Reader
import Data.Env.Types
import Html.Common.GitHub
import Html.JolHarg.Index
import Make
import Network.HTTP.Req
import Prelude
import Text.Blaze.Html5       as H hiding (main)


build ∷ WebsiteIO ()
build = do
  slug' <- asks slug
  _ <- liftIO $ loadFile defaultConfig
  reposJH <- liftIO . runReq defaultHttpConfig $ getRepos "jolharg"
  reposDan <- liftIO . runReq defaultHttpConfig $ getRepos "danwdart"
  let page' = runReader page (reposJH <> reposDan) :: Reader Website Html

  make slug' page' page404
