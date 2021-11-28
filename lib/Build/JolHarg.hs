{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE UnicodeSyntax     #-}

module Build.JolHarg where

import           Configuration.Dotenv
import           Control.Monad.IO.Class
import           Control.Monad.Trans.Reader
import           Data.Env.Types
import           Html.Common.GitHub
import           Html.JolHarg.Index
import           Make
import           Network.HTTP.Req
import           Prelude
import qualified Data.Text as T

build ∷ WebsiteIO ()
build = do
  slug' <- asks slug

  liftIO . putStrLn $ "Building " <> T.unpack slug'

  _ <- liftIO $ loadFile defaultConfig
  reposDan <- liftIO . runReq defaultHttpConfig $ getRepos "danwdart"
  reposJH <- liftIO . runReq defaultHttpConfig $ getRepos "jolharg"
  let page' = runReader page (reposDan <> reposJH)

  make slug' page' page404

serve ∷ WebsiteIO ()
serve = asks slug >>= makeServe Build.JolHarg.build
