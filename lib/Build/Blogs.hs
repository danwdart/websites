{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecursiveDo #-}

module Build.Blogs where

import Control.Monad (join)
import Control.Monad.IO.Class
import Control.Monad.Reader
import Data.ByteString.Char8         qualified as BS
import Data.Env.Types
import Data.Foldable
import Data.List.NonEmpty            (NonEmpty (..))
import Data.List.NonEmpty            qualified as LNE
import Data.Map                      (Map)
import Data.Map                      qualified as M
import Data.Maybe
-- import Data.Set                      (Set)
import Data.Set                      qualified as S
import Data.Text                     qualified as T
import Data.Text.IO                  qualified as TIO
import Data.Text.Encoding
import Data.Traversable
import Html.Common.Blog.Feed
import Html.Common.Blog.Link
import Html.Common.Blog.Post
import Html.Common.Blog.Types        qualified as BlogTypes
import Make
import System.Directory
import System.FilePath
import Data.Time.Clock
import Text.Blaze.Html.Renderer.Utf8 (renderHtml)
import Text.Blaze.Html5              as H hiding (main, title)
import Web.Sitemap.Gen

-- go through and write in some way, so something to concat perhaps

-- thanks chatgpt
groupByMany :: (Foldable f, Ord tag) => (post -> f tag) -> NonEmpty post -> Map tag (NonEmpty post)
groupByMany postToTags =
  foldMap (\post -> foldMap (\tag -> M.singleton tag (LNE.singleton post)) (postToTags post))

build ∷ (MonadReader Website m, MonadIO m) ⇒ (Html → Html → Html → m Html) → m Html → m ()
build page page404 = mdo
  url' <- asks url
  title' <- asks title
  slug' <- asks slug
  email' <- asks email
  -- Clear us out, Jim
  let prefix = ".sites/" <> T.unpack slug' <> "/"
  traverse_ (liftIO . removePathForcibly . (prefix <>)) [
    "post",
    "tag"
    ]
  (sortedPosts, renderedPosts) <- buildMD ("posts" </> T.unpack slug') email' (const mempty)
  -- By tag
  let grouped = groupByMany (S.fromList . LNE.toList . BlogTypes.tags . BlogTypes.metadata) sortedPosts :: Map BlogTypes.BlogTag (NonEmpty BlogTypes.BlogPost)

  let tags = fromJust (LNE.nonEmpty (M.keys grouped))

  tagUrlDates <- fromJust . LNE.nonEmpty . M.elems <$> M.traverseWithKey (\tag posts -> mdo
    postsRendered <- foldtraverse (renderPost email' (const mempty)) posts
    pageTag <- page (makeLinks sortedPosts) (makeTags tags) postsRendered
    let fullFilename = prefix <> "tag/" <> T.unpack (BlogTypes.getTag tag) <> "/index.html"
    let dirname = dropFileName fullFilename
    liftIO . createDirectoryIfMissing True $ dirname
    liftIO . BS.writeFile fullFilename . BS.toStrict . renderHtml $ pageTag
    liftIO . TIO.writeFile (".sites" </> T.unpack slug' </> "tag" </> T.unpack (BlogTypes.getTag tag) </> "atom.xml") $ makeRSSFeed (url' <> "/tag/" <> BlogTypes.getTag tag <> "/atom.xml") (url' <> "/tag/" <> BlogTypes.getTag tag) url' ("Posts tagged with " <> BlogTypes.getTag tag <> ": " <> title') posts
    -- liftIO . TIO.putStrLn $ url' <> "/tag/" <> BlogTypes.getTag tag
    -- liftIO . TIO.putStrLn . T.pack . show . BlogTypes.date . BlogTypes.metadata . LNE.head $ posts
    pure (
      url' <> "/tag/" <> BlogTypes.getTag tag,
      BlogTypes.date . BlogTypes.metadata . LNE.head $ posts
      )
    ) grouped
  -- By post
  urlDatePairsFromPages <- fmap join . for sortedPosts $ \post -> do
    let aliases' = BlogTypes.aliases . BlogTypes.metadata $ post
    for aliases' $ \alias -> do
      -- TODO template
      let fullFilename = ".sites/" <> T.unpack slug' <> "/post" <> alias <> "/index.html" -- </> ???
      let dirname = dropFileName fullFilename
      liftIO . createDirectoryIfMissing True $ dirname
      renderedPost <- renderPost email' (const mempty) post
      pageBlogPost <- page (makeLinks sortedPosts) (makeTags tags) renderedPost
      liftIO . BS.writeFile fullFilename . BS.toStrict . renderHtml $ pageBlogPost
      pure (
        url' <> "/post" <> T.pack alias,
        BlogTypes.date . BlogTypes.metadata $ post
        )
    
  now <- liftIO getCurrentTime
  let sitemap' = Sitemap $ [
        SitemapUrl url' (Just now) (Just Weekly) (Just 1.0)
        ] <> fmap (\(url, date) -> SitemapUrl url (Just date) (Just Never) (Just 1.0)) (LNE.toList urlDatePairsFromPages)
          <> fmap (\(url, date) -> SitemapUrl url (Just date) (Just Weekly) (Just 2.0)) (LNE.toList tagUrlDates)
  liftIO . BS.writeFile ( ".sites" </> T.unpack slug' </> "sitemap.xml") $ renderSitemap sitemap'
  liftIO . TIO.writeFile (".sites" </> T.unpack slug' </> "atom.xml") $ makeRSSFeed (url' <> "/atom.xml") url' url' title' sortedPosts
  liftIO . BS.writeFile ( ".sites" </> T.unpack slug' </> "robots.txt") $ "User-agent: *\nAllow: /\nSitemap: " <> encodeUtf8 url' <> "/sitemap.xml"
  make slug' (page (makeLinks sortedPosts) (makeTags tags) renderedPosts) page404
