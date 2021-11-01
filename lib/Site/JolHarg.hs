{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE UnicodeSyntax     #-}

module Site.JolHarg where

import           Configuration.Dotenv
import           Control.Monad.IO.Class
import           Control.Monad.Trans.Reader
import           Data.Env
import           Html.Common.GitHub
import           Html.JolHarg.Index
import           Network.HTTP.Req
import           Prelude                    hiding (putStrLn)
import           Util.Build                 (make, makeServe)

build ∷ WebsiteIO ()
build = do
  slug' <- asks slug

  _ <- liftIO $ loadFile defaultConfig
  reposDan <- liftIO . runReq defaultHttpConfig $ getRepos "danwdart"
  reposJH <- liftIO . runReq defaultHttpConfig $ getRepos "jolharg"
  let page' = runReader page (reposDan <> reposJH)

  make slug' page' page404

serve ∷ WebsiteIO ()
serve = asks slug >>= makeServe build