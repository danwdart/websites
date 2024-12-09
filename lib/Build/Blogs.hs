{-# LANGUAGE OverloadedStrings #-}

module Build.Blogs where

import Control.Monad (void)
import Control.Monad.IO.Class
import Control.Monad.Reader
import Data.ByteString.Char8         qualified as BS
import Data.Env.Types
import Data.Foldable
import Data.List.NonEmpty            (NonEmpty (..))
import Data.List.NonEmpty            qualified as LNE
import Data.Map                      (Map)
import Data.Map                      qualified as M
-- import Data.Set                      (Set)
import Data.Set                      qualified as S
import Data.Text                     qualified as T
import Data.Text.IO                  qualified as TIO
import Html.Common.Blog.Feed
import Html.Common.Blog.Link
import Html.Common.Blog.Post
import Html.Common.Blog.Types        qualified as BlogTypes
import Make
import System.Directory
import System.FilePath
import Text.Blaze.Html.Renderer.Utf8 (renderHtml)
import Text.Blaze.Html5              as H hiding (main, title)

-- go through and write in some way, so something to concat perhaps

-- thanks chatgpt
groupByMany :: (Foldable f, Ord tag) => (post -> f tag) -> NonEmpty post -> Map tag (NonEmpty post)
groupByMany postToTags =
  foldMap (\post -> foldMap (\tag -> M.singleton tag (LNE.singleton post)) (postToTags post))

build ∷ (MonadReader Website m, MonadIO m) ⇒ (Html → Html → m Html) → m Html → m ()
build page page404 = do
  url' <- asks url
  title' <- asks title
  slug' <- asks slug
  email' <- asks email
  (sortedPosts, renderedPosts) <- buildMD ("posts" </> T.unpack slug') email' (const mempty)
  -- By tag
  let grouped = groupByMany (S.fromList . BlogTypes.tags . BlogTypes.metadata) sortedPosts :: Map BlogTypes.BlogTag (NonEmpty BlogTypes.BlogPost)
  void $ M.traverseWithKey (\tag posts -> do
    postsRendered <- foldtraverse (renderPost email' (const mempty)) posts
    pageTag <- page (makeLinks sortedPosts) postsRendered
    let fullFilename = ".sites/" <> T.unpack slug' <> "/tag/" <> T.unpack (BlogTypes.getTag tag) <> ".html"
    let dirname = dropFileName fullFilename
    liftIO . createDirectoryIfMissing True $ dirname
    liftIO . BS.writeFile fullFilename . BS.toStrict . renderHtml $ pageTag
    ) grouped
  -- By post
  for_ sortedPosts $ \post -> do
    let aliases' = BlogTypes.aliases . BlogTypes.metadata $ post
    for_ aliases' $ \alias -> do
      -- TODO template
      let fullFilename = ".sites/" <> T.unpack slug' <> alias -- </> ???
      let dirname = dropFileName fullFilename
      liftIO . createDirectoryIfMissing True $ dirname
      renderedPost <- renderPost email' (const mempty) post
      pageBlogPost <- page (makeLinks sortedPosts) renderedPost
      liftIO . BS.writeFile fullFilename . BS.toStrict . renderHtml $ pageBlogPost

  liftIO . TIO.writeFile (".sites" </> T.unpack slug' </> "atom.xml") $ makeRSSFeed url' title' sortedPosts
  make slug' (page (makeLinks sortedPosts) renderedPosts) page404
