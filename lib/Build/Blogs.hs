{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecursiveDo       #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

module Build.Blogs where

import Control.Lens
import Control.Monad                 (join)
import Control.Monad.IO.Class
import Control.Monad.Reader
import Data.ByteString.Char8         qualified as BS
import Data.Env.Types                as Env
import Data.Foldable
import Data.Foldable1
import Data.List.NonEmpty            (NonEmpty (..))
import Data.List.NonEmpty            qualified as LNE
-- import Data.Map                      (Map)
-- import Data.Map                      qualified as M
import Data.Map.NonEmpty (NEMap)
import Data.Map.NonEmpty qualified as MNE
-- import Data.Maybe
-- import Data.Set                      (Set)
-- import Data.Set                      qualified as S
-- import Data.Set.NonEmpty                      (NESet)
import Data.Set.NonEmpty                      qualified as SNE
-- import Data.Text                     (Text)
import Data.Text                     qualified as T
import Data.Text.Encoding
import Data.Text.IO                  qualified as TIO
import Data.Time.Clock
import Data.Traversable
import Html.Common.Blog.Feed
import Html.Common.Blog.Link
import Html.Common.Blog.Post
import Html.Common.Blog.Types        qualified as BlogTypes
import Make
import System.Directory
import System.FilePath
import Text.Blaze.Html.Renderer.Utf8 (renderHtml)
import Text.Blaze.Html5              as H hiding (main, title)
import Web.Sitemap.Gen

-- go through and write in some way, so something to concat perhaps

-- thanks chatgpt
groupByMany ∷ (Foldable1 f, Ord tag) ⇒ (post → f tag) → NonEmpty post → NEMap tag (NonEmpty post)
groupByMany postToTags =
  foldMap1 (\post -> foldMap1 (\tag -> MNE.singleton tag (LNE.singleton post)) (postToTags post))

build ∷ (MonadReader Website m, MonadIO m) ⇒ (Html → Html → Html → m Html) → m Html → m ()
build page page404 = do
  baseUrl' <- view baseUrl
  title' <- view Env.title
  slug' <- view slug
  email' <- view email
  -- atomUrl' <- view $ siteType . atomUrl
  -- atomTitle' <- view $ siteType . atomTitle
  -- Clear us out, Jim
  let prefix = ".sites/" <> T.unpack slug' <> "/"
  traverse_ (liftIO . removePathForcibly . (prefix <>)) [
    "post",
    "tag"
    ]
  (sortedPosts, renderedPosts) <- buildMD ("posts" </> T.unpack slug') email' (const mempty)
  -- By tag
  let grouped = groupByMany (SNE.fromList . BlogTypes.tags . BlogTypes.metadata) sortedPosts :: NEMap BlogTypes.BlogTag (NonEmpty BlogTypes.BlogPost)

  let tags = MNE.keys grouped

  tagUrlDates <- MNE.elems <$> MNE.traverseWithKey (\tag posts -> mdo
    postsRendered <- foldtraverse (renderPost email' (const mempty)) posts
    -- TODO: lowercase earlier?

    pageTag <- local (\w@Website { _title = title'', _baseUrl, _siteType = Blog { _atomTitle = atomTitle' }} -> w {
      _title = ("Posts tagged with " <> BlogTypes.getTag tag <> ": ") <> title'',
      _siteType = Blog {
        _atomTitle = ("Posts tagged with " <> BlogTypes.getTag tag <> ": ") <> atomTitle',
        _atomUrl = baseUrl' <> "/tag/" <> encodeUtf8 (BlogTypes.getTag tag) <> "/atom.xml"
      }
    }) $
      page (makeLinks sortedPosts) (makeTags tags) postsRendered --  (("Posts tagged with " <> BlogTypes.getTag tag <> ": ") <>)
    let fullFilename = prefix <> "tag/" <> T.unpack (BlogTypes.getTag tag) <> "/index.html"
    let dirname = dropFileName fullFilename
    liftIO . createDirectoryIfMissing True $ dirname
    liftIO . BS.writeFile fullFilename . BS.toStrict . renderHtml $ pageTag
    liftIO . TIO.writeFile (".sites" </> T.unpack slug' </> "tag" </> T.unpack (BlogTypes.getTag tag) </> "atom.xml") $
      makeRSSFeed
        -- this should come from _atomUrl above
        (baseUrl' <> "/tag/" <> encodeUtf8 (BlogTypes.getTag tag) <> "/atom.xml")
        -- this should come from _pageUrl above
        (baseUrl' <> "/tag/" <> encodeUtf8 (BlogTypes.getTag tag))
        -- this is plain old base url
        baseUrl'
        -- and this should come from _pageTitle
        ("Posts tagged with " <> BlogTypes.getTag tag <> ": " <> title')
        posts
    -- liftIO . TIO.putStrLn $ url' <> "/tag/" <> BlogTypes.getTag tag
    -- liftIO . TIO.putStrLn . T.pack . show . BlogTypes.date . BlogTypes.metadata . LNE.head $ posts
    pure (
      decodeUtf8 (baseUrl' <> "/tag/" <> encodeUtf8 (BlogTypes.getTag tag)),
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
      pageBlogPost <- local (\w -> w {
        _title = ((BlogTypes.title . BlogTypes.metadata $ post) <> ": ") <> title'
      }) $
        page (makeLinks sortedPosts) (makeTags tags) renderedPost
      liftIO . BS.writeFile fullFilename . BS.toStrict . renderHtml $ pageBlogPost
      pure (
        decodeUtf8 (baseUrl' <> "/post" <> BS.pack alias),
        BlogTypes.date . BlogTypes.metadata $ post
        )

  now <- liftIO getCurrentTime
  let sitemap' = Sitemap $ [
        SitemapUrl (decodeUtf8 baseUrl') (Just now) (Just Weekly) (Just 1.0)
        ] <> fmap (\(url, date) -> SitemapUrl url (Just date) (Just Never) (Just 1.0)) (LNE.toList urlDatePairsFromPages)
          <> fmap (\(url, date) -> SitemapUrl url (Just date) (Just Weekly) (Just 2.0)) (LNE.toList tagUrlDates)
  liftIO . BS.writeFile ( ".sites" </> T.unpack slug' </> "sitemap.xml") $ renderSitemap sitemap'
  liftIO . TIO.writeFile (".sites" </> T.unpack slug' </> "atom.xml") $ makeRSSFeed (baseUrl' <> "/atom.xml") baseUrl' baseUrl' title' sortedPosts
  liftIO . BS.writeFile (".sites" </> T.unpack slug' </> "robots.txt") $ "User-agent: *\nAllow: /\nSitemap: " <> baseUrl' <> "/sitemap.xml"
  make slug' (page (makeLinks sortedPosts) (makeTags tags) renderedPosts) page404
