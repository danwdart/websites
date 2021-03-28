{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE UnicodeSyntax     #-}

module Site.JolHarg where

import Control.Monad (void)
import Control.Monad.IO.Class
import Control.Monad.Trans.Reader
import Data.Env
import           Util.Build                    (makeServe)
import           Configuration.Dotenv
import qualified Data.ByteString.Lazy.Char8    as BSL
import           Html.Common.GitHub
import           Html.JolHarg.Index
import           Network.HTTP.Req
import           System.Path
import           Text.Blaze.Html.Renderer.Utf8

build ∷ WebsiteIO ()
build = do
  page' <- page
  liftIO $ do
    void $ loadFile defaultConfig
    reposDan <- runReq defaultHttpConfig $ getRepos "danwdart"
    reposJH <- runReq defaultHttpConfig $ getRepos "jolharg"
    copyDir "static/common" ".sites/jolharg"
    copyDir "static/jolharg" ".sites/jolharg"
    BSL.writeFile ".sites/jolharg/index.html" . renderHtml $ runReader page' (reposDan <> reposJH)
    putStrLn "jolharg compiled."

serve ∷ WebsiteIO ()
serve = makeServe build "jolharg"
