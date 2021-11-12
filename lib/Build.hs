{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE UnicodeSyntax     #-}

module Build where

import qualified Build.Blog                     as B
import qualified Build.DanDart                  as D
import qualified Build.JolHarg                  as J
import qualified Build.M0ORI                    as M
import qualified Build.MadHacker                as R
import           Control.Monad
import           Control.Monad.IO.Class
import           Control.Monad.Trans.Reader
import qualified Data.ByteString.Lazy.Char8     as BSL
import           Data.Env
import           Data.List                      (sortOn)
import           Data.Map                       ((!))
import           Data.Maybe                     (fromMaybe, mapMaybe)
import           Data.Ord                       (Down (Down))
import           Data.Text                      (Text)
import qualified Data.Text                      as T
import           Data.Text.IO
import           Html.Common.Blog.Post
import           Html.Common.Blog.Types
import           Network.Wai.Application.Static
import           Network.Wai.Handler.Warp
import           Prelude                        hiding (putStrLn)
import           System.Directory               (doesFileExist,
                                                 getDirectoryContents)
import           System.Environment             (lookupEnv)
import           System.FilePath                ((</>))
import           System.Path
import           Text.Blaze.Html.Renderer.Utf8
import           Text.Blaze.Html5               as H hiding ((!))
import           WaiAppStatic.Types

--  TODO ask env
build ∷ WebsitesIO ()
build = do
    websites <- ask
    liftIO $ do
        runReaderT B.build $ websites ! "blog"
        runReaderT D.build $ websites ! "dandart"
        runReaderT J.build $ websites ! "jolharg"
        runReaderT M.build $ websites ! "m0ori"
        runReaderT R.build $ websites ! "madhacker"
