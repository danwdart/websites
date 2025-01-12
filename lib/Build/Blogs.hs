{-# LANGUAGE OverloadedStrings #-}
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
import Data.Map                      qualified as M
import Data.Map.NonEmpty             (NEMap)
import Data.Map.NonEmpty             qualified as MNE
import Data.Maybe
-- import Data.Set                      (Set)
-- import Data.Set                      qualified as S
-- import Data.Set.NonEmpty                      (NESet)
import Data.Set.NonEmpty             qualified as SNE
-- import Data.Text                     (Text)
import Data.Text                     qualified as T
import Data.Text.IO                  qualified as TIO
import Data.Time.Clock
import Data.Traversable
import Html.Common.Blog.Feed
import Html.Common.Blog.Link
import Html.Common.Blog.Post
import Html.Common.Blog.Types        qualified as BlogTypes
import Make
import Network.URI
import System.Directory
import System.FilePath
import Text.Blaze.Html.Renderer.Utf8 (renderHtml)
import Text.Blaze.Html5              as H hiding (main, title)
import Web.Sitemap.Gen

-- go through and write in some way, so something to concat perhaps

-- urgh
groupByMany ∷ (Foldable1 f, Ord tag) ⇒ (post → f tag) → NonEmpty post → NEMap tag (NonEmpty post)
groupByMany postToTags posts = MNE.unsafeFromMap $
  foldr' (\post map'' ->
    foldr' (\tag map' ->
        M.insertWith (<>) tag (LNE.singleton post) map'
      )
      map''
      (postToTags post)
    )
    M.empty
    posts

build ∷ (MonadReader Website m, MonadIO m) ⇒ (Html → Html → Html → m Html) → m Html → m ()
build page page404 = do
  baseUrl' <- view baseUrl
  title' <- view Env.title
  slug' <- view slug
  mAtomUri' <- preview $ siteType . atomUrl
  sitemapUrl' <- view sitemapUrl
  let atomUri' = fromJust mAtomUri' -- no Monoid for URI
  -- atomTitle' <- view $ siteType . atomTitle
  -- Clear us out, Jim
  let siteDir = ".sites/" <> T.unpack slug' <> "/"
  traverse_ (liftIO . removePathForcibly . (siteDir <>)) [
    "post",
    "tag"
    ]
  (sortedPosts, renderedPosts) <- buildMD ("posts" </> T.unpack slug')
  -- By tag
  let grouped = groupByMany (SNE.fromList . BlogTypes.tags . BlogTypes.metadata) sortedPosts :: NEMap BlogTypes.BlogTag (NonEmpty BlogTypes.BlogPost)
  let tags = MNE.keys grouped

  -- pretty sus of this
  tagUrlDates <- MNE.elems <$> MNE.traverseWithKey (\tag posts -> do
    -- Okay LNE.filter does not have this guarantee - anything else?
    postsRendered <- foldtraverse renderPost posts
    -- TODO: lowercase earlier?

    let relTagUri = fromJust . parseRelativeReference $ "/tag/" <> escapeURIString isUnescapedInURIComponent (T.unpack (BlogTypes.getTag tag))
    let relAtomUri = fromJust . parseRelativeReference $ "/atom.xml"
    let tagUri' = relTagUri `relativeTo` baseUrl'
    let tagAtomUri' = relAtomUri `relativeTo` tagUri'

    let atomDesc = "Posts tagged with " <> BlogTypes.getTag tag
    let atomPrefix = atomDesc <> ": "
    let atomPrefixer = (atomPrefix <>)
    let fullAtomTitle' = atomPrefixer title'

    let atomFilename = ".sites" </> T.unpack slug' </> "tag" </> T.unpack (BlogTypes.getTag tag) </> "atom.xml"
    let fullFilename = siteDir <> "tag/" <> T.unpack (BlogTypes.getTag tag) <> "/index.html"
    let dirname = dropFileName fullFilename

    pageTag <- local (\w@Website { _title = title'', _baseUrl, _siteType = siteType'@Blog { _atomTitle = atomTitle' }} -> w {
      _title = atomPrefixer title'',
      _siteType = siteType' {
        _atomTitle = atomPrefixer atomTitle',
        _atomUrl = tagAtomUri'
      }
    }) . addBreadcrumb atomDesc $
      page (makeLinks Nothing "#" atomDesc posts) (makeTags (Just tag) tags) postsRendered --  (("Posts tagged with " <> BlogTypes.getTag tag <> ": ") <>)

    liftIO . createDirectoryIfMissing True $ dirname
    liftIO . BS.writeFile fullFilename . BS.toStrict . renderHtml $ pageTag
    liftIO . TIO.writeFile atomFilename $ makeRSSFeed tagAtomUri' tagUri' baseUrl' fullAtomTitle' posts
    -- liftIO . TIO.putStrLn $ "/tag/" <> BlogTypes.getTag tag
    -- traverse_ (liftIO . TIO.putStrLn . BlogTypes.title . BlogTypes.metadata) posts
    pure (
      tagUri',
      BlogTypes.date . BlogTypes.metadata . LNE.head $ posts
      )
    ) grouped
  -- By post
  urlDatePairsFromPages <- fmap join . for sortedPosts $ \post -> do
    let aliases' = BlogTypes.aliases . BlogTypes.metadata $ post
    for aliases' $ \alias -> do
      let fullFilename = siteDir <> "post" <> alias <> "/index.html" -- </> ???
      let dirname = dropFileName fullFilename
      let aliasSuffix = fromJust . parseRelativeReference $ alias
      let aliasUrl = aliasSuffix `relativeTo` baseUrl'
      let postTitle = BlogTypes.title . BlogTypes.metadata $ post
      let postTitlePrefix = postTitle <> ": "
      let postTitlePrefixer = (postTitlePrefix <>)

      liftIO . createDirectoryIfMissing True $ dirname
      renderedPost <- renderPost post
      pageBlogPost <- local (\w -> w {
        -- we don't override rss title, only page title, this is why they're separate
        _title = postTitlePrefixer title',
        _openGraphInfo = OGArticle $ OpenGraphArticle {
            _ogArticlePublishedTime = BlogTypes.date . BlogTypes.metadata $ post,
            _ogArticleModifiedTime = Just . BlogTypes.date . BlogTypes.metadata $ post,
            _ogArticleExpirationTime = Nothing,
            _ogArticleAuthor =
              OpenGraphProfile {
                _ogProfileFirstName = "Dan",
                _ogProfileLastName = "Dart",
                _ogProfileUsername = "dandart",
                _ogProfileGender = "non-binary"
              } :| [],
            _ogArticleSection = "Blog post",
            _ogArticleTag = BlogTypes.tags . BlogTypes.metadata $ post
        },
        _previewImgUrl = maybe (w ^. previewImgUrl) (fromJust . parseURI . T.unpack) . BlogTypes.featuredImage . BlogTypes.metadata $ post
      }) . addBreadcrumb (BlogTypes.title . BlogTypes.metadata $ post) $
        page (makeLinks (Just . BlogTypes.postId $ post) "/#" "All Posts" sortedPosts) (makeTags Nothing tags) renderedPost
      liftIO . BS.writeFile fullFilename . BS.toStrict . renderHtml $ pageBlogPost
      pure (
        aliasUrl,
        BlogTypes.date . BlogTypes.metadata $ post
        )

  now <- liftIO getCurrentTime
  let sitemap' = Sitemap $ [
        SitemapUrl (T.pack . show $ baseUrl') (Just now) (Just Weekly) (Just 1.0)
        ] <> fmap (\(url, date) -> SitemapUrl (T.pack . show $ url) (Just date) (Just Never) (Just 1.0)) (LNE.toList urlDatePairsFromPages)
          <> fmap (\(url, date) -> SitemapUrl (T.pack . show $ url) (Just date) (Just Weekly) (Just 2.0)) (LNE.toList tagUrlDates)
  liftIO . BS.writeFile (siteDir <> "/sitemap.xml") $ renderSitemap sitemap'
  liftIO . TIO.writeFile (siteDir <> "atom.xml") $
    makeRSSFeed atomUri' baseUrl' baseUrl' title' sortedPosts
  liftIO . BS.writeFile (siteDir <> "/robots.txt") $
    "User-agent: *\nAllow: /\nSitemap: " <> BS.pack (show sitemapUrl')
  make slug' (page (makeLinks Nothing "#" "All Posts" sortedPosts) (makeTags Nothing tags) renderedPosts) page404
