{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Unsafe            #-}
{-# OPTIONS_GHC -Wno-unsafe #-}

module Make where

import Control.Monad
import Control.Monad.Reader
import Data.ByteString.Lazy.Char8    qualified as BSL
import Data.Env.Types
import Data.Foldable
import Data.List                     (sortOn)
import Data.List.NonEmpty            (NonEmpty)
import Data.List.NonEmpty            qualified as LNE
import Data.Ord                      (Down (Down))
import Data.Text                     (Text)
import Data.Text                     qualified as T
import Data.Text.IO                  qualified as TIO
import Html.Common.Blog.Post
import Html.Common.Blog.Types
import System.Directory              (doesFileExist, getDirectoryContents)
import System.FilePath               ((</>))
import System.Path
import Text.Blaze.Html.Renderer.Utf8
import Text.Blaze.Html5              as H

make ∷ (MonadReader Website m, MonadIO m) ⇒ Text → m Html → m Html → m ()
make name page page404 = do
    let path = T.unpack name
    page' <- page
    page404' <- page404
    liftIO $ do
        copyDir "static/common" $ ".sites" </> path
        copyDir ("static" </> path) (".sites" </> path)
        BSL.writeFile (".sites" </> path </> "index.html") $ renderHtml page'
        BSL.writeFile (".sites" </> path </> "404.html") $ renderHtml page404'
        TIO.putStrLn $ name <> " compiled."

foldtraverse ∷ (Monoid b', Traversable t, Applicative f) ⇒ (a' → f b') → t a' → f b'
foldtraverse f xs = fold <$> traverse f xs

buildMD ∷ forall m. (MonadReader Website m, MonadIO m) ⇒ FilePath → Text → (BlogMetadata → Html) → m (NonEmpty BlogPost, Html)
buildMD postsDir email' renderSuffix = do
  files' <- liftIO $ getDirectoryContents postsDir
  let fileNames = (postsDir </>) <$> files' -- if used in same line, use Compose
  validFiles <- liftIO $ filterM doesFileExist fileNames
  posts <- liftIO (traverse (makeBlogPost postsDir) validFiles)
  let sortedPosts = sortOn (Down . date . metadata) . filter (not . draft . metadata) $ posts
  case LNE.nonEmpty sortedPosts of
    Nothing -> liftIO $ fail "No valid, non-draft posts."
    Just sortedPosts' -> do
      renderedPosts <- foldtraverse (renderPost email' renderSuffix) sortedPosts'
      pure (sortedPosts', renderedPosts)
