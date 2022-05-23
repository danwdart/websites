{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Make where

import           Control.Monad
import           Control.Monad.Reader
import qualified Data.ByteString.Lazy.Char8     as BSL
import           Data.Env.Types
import           Data.List                      (sortOn)
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
import           Text.Blaze.Html5               as H
import           WaiAppStatic.Types

make ∷ Text → WebsiteM Html → WebsiteM Html → WebsiteIO ()
make name page page404 = do
    let path = T.unpack name
    page' <- websiteMToWebsiteIO page
    page404' <- websiteMToWebsiteIO page404
    liftIO $ do
        copyDir "static/common" $ ".sites" </> path
        copyDir ("static" </> path) (".sites" </> path)
        BSL.writeFile (".sites" </> path </> "index.html") $ renderHtml page'
        BSL.writeFile (".sites" </> path </> "404.html") $ renderHtml page404'
        putStrLn $ name <> " compiled."

makeServe ∷ (MonadReader Website m, MonadIO m) ⇒ m () → Text → m ()
makeServe build' slug' = do
    liftIO $ putStrLn "Building..."
    build'
    liftIO $ do
        port <- fromMaybe "80" <$> lookupEnv "PORT"
        putStrLn $ "Serving on http://localhost:" <> T.pack port
        runEnv 80 . staticApp $ (defaultWebAppSettings $ ".sites/" <> T.unpack slug' <> "/"){ ssIndices = mapMaybe toPiece ["index.html"] }

buildMD ∷ FilePath → Text → (BlogMetadata → Html) → WebsiteIO ([BlogPost], Html)
buildMD postsDir postType renderSuffix = do
  files' <- liftIO $ getDirectoryContents postsDir
  let fileNames = (postsDir </>) <$> files' -- if used in same line, use Compose
  validFiles <- liftIO $ filterM doesFileExist fileNames
  posts <- liftIO . sequence $ makeBlogPost postsDir <$> validFiles
  let sortedPosts = sortOn (Down . date . metadata) . filter (not . draft . metadata) $ posts
  renderedPosts <- websiteMToWebsiteIO $ foldMap (renderPost postType renderSuffix) sortedPosts

  pure (sortedPosts, renderedPosts)
