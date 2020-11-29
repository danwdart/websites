{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE UnicodeSyntax     #-}

module Site.JolHarg where

import           Configuration.Dotenv
import           Control.Monad.Reader

import qualified Data.ByteString.Lazy.Char8     as BSL

import           Data.Maybe

import           Html.Common.GitHub

import           Html.JolHarg.Index

import           Network.HTTP.Req

import           Network.Wai.Application.Static
import           Network.Wai.Handler.Warp

import           System.Path

import           Text.Blaze.Html.Renderer.Utf8

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
serve = do
    putStrLn "Building..."
    build
    putStrLn "Serving..."
    runEnv 80 . staticApp $ (defaultWebAppSettings ".sites/jolharg/"){ ssIndices = mapMaybe toPiece ["index.html"] }
