{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE UnicodeSyntax     #-}

module Site.JolHarg where

import           Configuration.Dotenv
import           Control.Monad                 (void)
import           Control.Monad.IO.Class
import           Control.Monad.Trans.Reader
import qualified Data.ByteString.Lazy.Char8    as BSL
import           Data.Env
import           Html.Common.GitHub
import           Html.JolHarg.Index
import           Network.HTTP.Req
import           System.Path
import           Text.Blaze.Html.Renderer.Utf8
import           Util.Build                    (makeServe)

build ∷ WebsiteIO ()
build = do
  liftIO $ do
    void $ loadFile defaultConfig
    copyDir "static/common" ".sites/jolharg"
    copyDir "static/jolharg" ".sites/jolharg"
  reposDan <- liftIO . runReq defaultHttpConfig $ getRepos "danwdart"
  reposJH <- liftIO . runReq defaultHttpConfig $ getRepos "jolharg"
  page' <- websiteMToWebsiteIO $ runReader page (reposDan <> reposJH)
  liftIO $ do
    BSL.writeFile ".sites/jolharg/index.html" . renderHtml $ page'
    putStrLn "jolharg compiled."

serve ∷ WebsiteIO ()
serve = makeServe build "jolharg"
