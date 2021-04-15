{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE UnicodeSyntax     #-}

module Util.Build (mkdirp, make, makeServe) where

import           Blog.Feed
import           Blog.Link
import           Blog.Post
import           Blog.Types                     as Blog
import           Configuration.Dotenv
import           Control.Monad                  (filterM, void)
import           Control.Monad.IO.Class
import           Control.Monad.Trans.Reader
import qualified Data.ByteString.Lazy.Char8     as BSL
import           Data.Env                       as Env
import           Data.List
import           Data.Maybe                     (fromMaybe, mapMaybe)
import           Data.Ord
import           Data.Text                      (Text)
import qualified Data.Text                      as T
import           Data.Text.IO                   as TIO
import           Html.Common.GitHub
import Html.MadHacker.Suffix
import           Network.HTTP.Req
import           Network.Wai.Application.Static
import           Network.Wai.Handler.Warp
import           Prelude                        hiding (putStrLn)
import           System.Directory
import           System.Environment             (lookupEnv)
import           System.FilePath
import           System.Path
import           Text.Blaze.Html.Renderer.Utf8
import           Text.Blaze.Html5               (Html)
import           WaiAppStatic.Types

buildMD ∷ FilePath → Text → (BlogMetadata → Html) → WebsiteIO ([BlogPost], Html)
buildMD postsDir postType renderSuffix = do
  files' <- liftIO $ getDirectoryContents postsDir
  let fileNames = (postsDir </>) <$> files' -- if used in same line, use Compose
  validFiles <- liftIO $ filterM doesFileExist fileNames
  posts <- liftIO . sequence $ makeBlogPost postsDir <$> validFiles
  let sortedPosts = sortOn (Down . date . metadata) . filter (not . draft . metadata) $ posts
  renderedPosts <- websiteMToWebsiteIO $ foldMap (renderPost postType renderSuffix) sortedPosts

  pure (sortedPosts, renderedPosts)

-- TODO get page?
buildBlog ∷ (Html -> Html -> WebsiteM Html) → WebsiteM Html → WebsiteIO ()
buildBlog page page404 = do
  url' <- asks url
  title' <- asks Env.title
  slug' <- asks slug
  -- TODO get these and merge with MH
  (sortedPosts, renderedPosts) <- buildMD "posts" "post" (const mempty)
  liftIO . TIO.writeFile (".sites" </> T.unpack slug' </> "atom.xml") $ makeRSSFeed url' title' sortedPosts
  make slug' (page (makeLinks sortedPosts) renderedPosts) page404

buildJH ∷ Reader [Repo] (WebsiteM Html) → Html → WebsiteIO ()
buildJH page page404 = do
  slug' <- asks slug
  -- TODO pull these out
  repos <- liftIO . runReq defaultHttpConfig $ do
      sequence $ getRepos <$> ["jolharg", "dandart"]
  let page' = runReader page (concat repos)
  make slug' page' (pure page404)

buildSite ∷ Html → Html → WebsiteIO ()
buildSite page page404 = do
    slug' <- asks slug
    make slug' (pure page) (pure page404)

buildMH ∷ (Html -> Html -> WebsiteM Html) → Html → WebsiteIO ()
buildMH page page404 = do
  slug' <- asks slug
  url' <- asks url
  title' <- asks Env.title
  -- TODO get these and merge with blog
  (sortedPosts, renderedPosts) <- buildMD "reviews" "review" renderStars
  liftIO . TIO.writeFile (".sites" </> T.unpack slug' </> "atom.xml") $ makeRSSFeed url' title' sortedPosts
  make slug' (page (makeLinks sortedPosts) renderedPosts) (pure page404)

serveSite ∷ WebsiteIO () -> WebsiteIO ()
serveSite build = do
    slug' <- asks slug
    -- TODO find correct build
    makeServe build slug'

mkdirp ∷ String → IO ()
mkdirp = createDirectoryIfMissing True

make ∷ Text → WebsiteM Html → WebsiteM Html → WebsiteIO ()
make name page page404 = do
    let path = T.unpack name
    page' <- websiteMToWebsiteIO page
    page404' <- websiteMToWebsiteIO page404
    liftIO $ do
        void $ loadFile defaultConfig
        copyDir "static/common" $ ".sites" </> path
        copyDir ("static" </> path) (".sites" </> path)
        BSL.writeFile (".sites" </> path </> "index.html") $ renderHtml page'
        BSL.writeFile (".sites" </> path </> "404.html") $ renderHtml page404'
        putStrLn $ name <> " compiled."

makeServe ∷ WebsiteIO () → Text → WebsiteIO ()
makeServe build slug' = do
    liftIO $ putStrLn "Building..."
    build
    liftIO $ do
        port <- fromMaybe "80" <$> lookupEnv "PORT"
        putStrLn $ "Serving on http://localhost:" <> T.pack port
        runEnv 80 . staticApp $ (defaultWebAppSettings $ ".sites/" <> T.unpack slug' <> "/"){ ssIndices = mapMaybe toPiece ["index.html"] }
