{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE UnicodeSyntax     #-}

module Site.JolHarg where

import           Configuration.Dotenv
import           Control.Monad                 (void)
import           Control.Monad.IO.Class
import           Control.Monad.Trans.Reader
import qualified Data.ByteString.Lazy.Char8    as BSL
import           Data.Env
import qualified Data.Text                     as T
import           Data.Text.IO
import           Html.Common.GitHub
import           Html.JolHarg.Index
import           Network.HTTP.Req
import           Prelude                       hiding (putStrLn)
import           System.FilePath
import           System.Path
import           Text.Blaze.Html.Renderer.Utf8
import           Util.Build                    (makeServe)

build ∷ WebsiteIO ()
build = do
  slug' <- asks slug
  liftIO $ do
    void $ loadFile defaultConfig
    copyDir "static/common" ".sites/jolharg"
    copyDir "static/jolharg" ".sites/jolharg"
  reposDan <- liftIO . runReq defaultHttpConfig $ getRepos "danwdart"
  reposJH <- liftIO . runReq defaultHttpConfig $ getRepos "jolharg"
  page' <- websiteMToWebsiteIO $ runReader page (reposDan <> reposJH)
  liftIO $ do
    BSL.writeFile (".sites" </> T.unpack slug' </> "index.html") . renderHtml $ page'
    putStrLn $ slug' <> " compiled."

serve ∷ WebsiteIO ()
serve = do
  slug' <- asks slug
  makeServe build slug'
