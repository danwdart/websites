{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE UnicodeSyntax     #-}

module Site.JolHarg where

import           Build.Utils                   (makeServe)
import           Configuration.Dotenv
import           Control.Monad.Reader
import qualified Data.ByteString.Lazy.Char8    as BSL
import           Html.Common.GitHub
import           Html.JolHarg.Index
import           Network.HTTP.Req
import           System.Path
import           Text.Blaze.Html.Renderer.Utf8

build ∷ Bool -> IO ()
build dev = do
    void $ loadFile defaultConfig
    reposDan <- runReq defaultHttpConfig $ getRepos "danwdart"
    reposJH <- runReq defaultHttpConfig $ getRepos "jolharg"
    copyDir "static/common" ".sites/jolharg"
    copyDir "static/jolharg" ".sites/jolharg"
    BSL.writeFile ".sites/jolharg/index.html" . renderHtml $ runReader (page dev) (reposDan <> reposJH)
    putStrLn "jolharg compiled."

serve ∷ Bool -> IO ()
serve dev = makeServe (build dev) "jolharg"
