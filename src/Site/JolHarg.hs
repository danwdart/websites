{-# LANGUAGE OverloadedStrings #-}

module Site.JolHarg where

import Build.Utils

import Control.Monad.Reader

import qualified Data.ByteString.Lazy.Char8 as BSL

import Data.Maybe

import Data.JolHarg

import Html.Common.Card
import Html.Common.GitHub
import Html.Common.Head

import Html.JolHarg.Index

import Network.HTTP.Req

import Network.Wai.Application.Static
import Network.Wai.Handler.Warp

import System.Directory
import System.Path

import Text.Blaze.Html.Renderer.Utf8
import Text.Blaze.Html5 as H hiding (main)
import Text.Blaze.Html5.Attributes as A

import WaiAppStatic.Types

build :: IO ()
build = do
    reposDan <- runReq defaultHttpConfig $ getRepos "danwdart"
    reposJH <- runReq defaultHttpConfig $ getRepos "jolharg"
    copyDir "static/common" ".sites/jolharg"
    copyDir "static/jolharg" ".sites/jolharg"
    BSL.writeFile ".sites/jolharg/index.html" $ renderHtml $ runReader page (reposDan <> reposJH)
    putStrLn "jolharg compiled."

serve :: IO ()
serve = do
    putStrLn "Building..."
    build
    putStrLn "Serving..."
    runEnv 80 . staticApp $ (defaultWebAppSettings ".sites/jolharg/"){ ssIndices = mapMaybe toPiece ["index.html"] } 