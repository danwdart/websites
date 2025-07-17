{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Unsafe            #-}
{-# OPTIONS_GHC -Wno-unsafe #-}

module Make where

import Control.Monad
import Control.Monad.Error.Class
import Control.Monad.Reader
import Data.ByteString.Char8         qualified as BS
import Data.Env.Types
import Data.Foldable
import Data.List                     (sortOn)
import Data.List.NonEmpty            (NonEmpty)
import Data.List.NonEmpty            qualified as LNE
import Data.NonEmpty                 qualified as NE
import Data.Ord                      (Down (Down))
import Data.Text                     qualified as T
import Data.Text.IO                  qualified as TIO
import Data.Text.NonEmpty            qualified as TNE
import Distribution.Simple.Utils
import Distribution.Verbosity
import Html.Common.Blog.Post
import Html.Common.Blog.Types
import System.Directory              (doesFileExist, getDirectoryContents)
import System.FilePath               ((</>))
-- import System.Path
import Text.Blaze.Html.Renderer.Utf8
import Text.Blaze.Html5              as H

make ∷ (MonadReader Website m, MonadIO m) ⇒ TNE.NonEmptyText → m Html → m Html → m ()
make name page page404 = do
    let path = T.unpack (NE.getNonEmpty name)
    page' <- page
    page404' <- page404
    liftIO $ do
        copyDirectoryRecursive silent  "static/common" (".sites" </> path)
        copyDirectoryRecursive silent ("static" </> path) (".sites" </> path)
        BS.writeFile (".sites" </> path </> "index.html") . BS.toStrict $ renderHtml page'
        BS.writeFile (".sites" </> path </> "404.html") . BS.toStrict $ renderHtml page404'
        TIO.putStrLn $ NE.getNonEmpty name <> " compiled."

foldtraverse ∷ (Monoid b', Traversable t, Applicative f) ⇒ (a' → f b') → t a' → f b'
foldtraverse f xs = fold <$> traverse f xs

buildMD ∷ forall m. (MonadReader Website m, MonadIO m) ⇒ FilePath → m (NonEmpty BlogPost, Html)
buildMD postsDir = do
  files' <- liftIO $ getDirectoryContents postsDir
  let fileNames = (postsDir </>) <$> files' -- if used in same line, use Compose
  validFiles <- liftIO $ filterM doesFileExist fileNames
  -- move this modify UP!
  posts <- liftIO (traverse (modifyError (userError . show) . makeBlogPost postsDir) validFiles)
  let sortedPosts = sortOn (Down . date . metadata) . filter (not . draft . metadata) $ posts
  case LNE.nonEmpty sortedPosts of
    -- TODO throw here instead
    Nothing -> liftIO $ fail "No valid, non-draft posts. Oh dear."
    Just sortedPosts' -> do
      -- potentially we could do this afterwards?
      renderedPosts <- foldtraverse renderPost sortedPosts'
      pure (sortedPosts', renderedPosts)
