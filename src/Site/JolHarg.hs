{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE UnicodeSyntax     #-}

module Site.JolHarg where

import           Build.Utils                    (makeServe)
import           Configuration.Dotenv
import           Control.Monad.Reader

import qualified Data.ByteString.Lazy.Char8     as BSL

import           Data.Maybe                     (fromMaybe, mapMaybe)

import           Html.Common.GitHub

import           Html.JolHarg.Index

import           Network.HTTP.Req

import           Network.Wai.Application.Static
import           Network.Wai.Handler.Warp

import           System.Path

import           Text.Blaze.Html.Renderer.Utf8

import           System.Environment             (lookupEnv)
import           WaiAppStatic.Types

build ∷ IO ()
build = do
    void $ loadFile defaultConfig
    reposDan <- runReq defaultHttpConfig $ getRepos "danwdart"
    reposJH <- runReq defaultHttpConfig $ getRepos "jolharg"
    copyDir "static/common" ".sites/jolharg"
    copyDir "static/jolharg" ".sites/jolharg"
    BSL.writeFile ".sites/jolharg/index.html" . renderHtml $ runReader page (reposDan <> reposJH)
    putStrLn "jolharg compiled."

serve ∷ IO ()
serve = makeServe build "jolharg"