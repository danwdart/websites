{-# LANGUAGE OverloadedStrings #-}
-- {-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

module Build.Blogs where

import Control.Exception.MissingAtomURIException
import Control.Lens
import Control.Monad                 (join)
import Control.Monad.Error.Class
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
import Data.NonEmpty                 qualified as NE
-- import Data.Set                      (Set)
-- import Data.Set                      qualified as S
-- import Data.Set.NonEmpty                      (NESet)
import Data.Set.NonEmpty             qualified as SNE
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

build ∷ (MonadReader Website m, MonadError MissingAtomURIException m, MonadIO m) ⇒ (Html → Html → Html → m Html) → m Html → m ()
build page page404 = do
  baseUrl' <- view baseUrl
  title' <- view Env.title
  slug' <- view slug
  mAtomUri' <- preview $ siteType . atomUrl
  sitemapUrl' <- view sitemapUrl
  atomUri' <- case mAtomUri' of
      Just x  -> pure x
      Nothing -> throwError MissingAtomURIException -- no Monoid for URI -- is that right?
  -- atomTitle' <- view $ siteType . atomTitle
  -- Clear us out, Jim
  let siteDir = ".sites/" <> T.unpack (NE.getNonEmpty slug') <> "/"
  traverse_ (liftIO . removePathForcibly . (siteDir <>)) [
    "post",
    "tag"
    ]
  (sortedPosts, renderedPosts) <- buildMD ("posts" </> T.unpack (NE.getNonEmpty slug'))
  -- By tag
  let grouped = groupByMany (SNE.fromList . BlogTypes.tags . BlogTypes.metadata) sortedPosts :: NEMap BlogTypes.BlogTag (NonEmpty BlogTypes.BlogPost)
  let tags = MNE.keys grouped

  -- pretty sus of this
  tagUrlDates <- MNE.elems <$> MNE.traverseWithKey (\tag posts -> do
    -- Okay LNE.filter does not have this guarantee - anything else?
    postsRendered <- foldtraverse renderPost posts
    -- TODO: lowercase earlier?

    let relTagUri = fromJust . parseRelativeReference $ "/tag/" <> escapeURIString isUnescapedInURIComponent (T.unpack (NE.getNonEmpty (BlogTypes.getTag tag)))
    let relAtomUri = fromJust . parseRelativeReference $ "/atom.xml"
    let tagUri' = relTagUri `relativeTo` baseUrl'
    let tagAtomUri' = relAtomUri `relativeTo` tagUri'

    -- TODO nonempty th?
    let atomDesc = NE.trustedNonEmpty "Posts tagged with " <> BlogTypes.getTag tag
    let atomPrefix = atomDesc <> NE.trustedNonEmpty ": "
    let atomPrefixer = (atomPrefix <>)
    let fullAtomTitle' = atomPrefixer title'

    let atomFilename = ".sites" </> T.unpack (NE.getNonEmpty slug') </> "tag" </> T.unpack (NE.getNonEmpty (BlogTypes.getTag tag)) </> "atom.xml"
    let fullFilename = siteDir <> "tag/" <> T.unpack (NE.getNonEmpty (BlogTypes.getTag tag)) <> "/index.html"
    let dirname = dropFileName fullFilename

    pageTag <- locally title atomPrefixer .
      locally (siteType . atomTitle) atomPrefixer .
      local (set (siteType . atomUrl) tagAtomUri') .
      addBreadcrumb atomDesc $
      page (makeLinks Nothing (NE.trustedNonEmpty "#") atomDesc posts) (makeTags (Just tag) tags) postsRendered --  (("Posts tagged with " <> BlogTypes.getTag tag <> ": ") <>)

    liftIO . createDirectoryIfMissing True $ dirname
    liftIO . BS.writeFile fullFilename . BS.toStrict . renderHtml $ pageTag
    case makeRSSFeed tagAtomUri' tagUri' baseUrl' fullAtomTitle' posts of
        Just rssFeed -> liftIO . TIO.writeFile atomFilename . NE.getNonEmpty $ rssFeed
        Nothing -> liftIO . putStrLn $ "No RSS feed - todo error"
    -- liftIO . TIO.putStrLn $ "/tag/" <> BlogTypes.getTag tag
    -- traverse_ (liftIO . TIO.putStrLn . BlogTypes.title . BlogTypes.metadata) posts
    pure (
      tagUri',
      (BlogTypes.date . BlogTypes.metadata)
      (LNE.head posts)
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
      let postTitlePrefix = postTitle <> NE.trustedNonEmpty ": "
      let postTitlePrefixer = (postTitlePrefix <>)

      liftIO . createDirectoryIfMissing True $ dirname
      renderedPost <- renderPost post
      pageBlogPost <- locally title postTitlePrefixer .
        local (set openGraphInfo (OGArticle $ OpenGraphArticle {
            _ogArticlePublishedTime = BlogTypes.date . BlogTypes.metadata $ post,
            _ogArticleModifiedTime = Just . BlogTypes.date . BlogTypes.metadata $ post,
            _ogArticleExpirationTime = Nothing,
            _ogArticleAuthor =
              OpenGraphProfile {
                _ogProfileFirstName = NE.trustedNonEmpty "Dan",
                _ogProfileLastName = NE.trustedNonEmpty "Dart",
                _ogProfileUsername = NE.trustedNonEmpty "dandart",
                _ogProfileGender = NE.trustedNonEmpty "non-binary"
              } :| [],
            _ogArticleSection = NE.trustedNonEmpty "Blog post",
            _ogArticleTag = BlogTypes.tags . BlogTypes.metadata $ post
        })) .
        local (\w -> w {
          -- we don't override rss title, only page title, this is why they're separate
          -- TODO maybe id -> fromMaybe? 
          _previewImgUrl = maybe (w ^. previewImgUrl) id (BlogTypes.featuredImage . BlogTypes.metadata $ post)
        }) . addBreadcrumb (BlogTypes.title . BlogTypes.metadata $ post) $
        page (makeLinks (Just . BlogTypes.postId $ post) (NE.trustedNonEmpty "/#") (NE.trustedNonEmpty "All Posts") sortedPosts) (makeTags Nothing tags) renderedPost
      liftIO . BS.writeFile fullFilename . BS.toStrict . renderHtml $ pageBlogPost
      pure (
        aliasUrl,
        BlogTypes.date . BlogTypes.metadata $ post
        )

  now <- liftIO getCurrentTime
  let sitemap' = Sitemap $ [
        SitemapUrl (T.show baseUrl') (Just now) (Just Weekly) (Just 1.0)
        ] <> fmap (\(url, date) -> SitemapUrl (T.show url) (Just date) (Just Never) (Just 1.0)) (LNE.toList urlDatePairsFromPages)
          <> fmap (\(url, date) -> SitemapUrl (T.show url) (Just date) (Just Weekly) (Just 2.0)) (LNE.toList tagUrlDates)
  liftIO . BS.writeFile (siteDir <> "/sitemap.xml") $ renderSitemap sitemap'
  
  case makeRSSFeed atomUri' baseUrl' baseUrl' title' sortedPosts of
    Just rssFeed' -> liftIO . TIO.writeFile (siteDir <> "atom.xml") . NE.getNonEmpty $ rssFeed'
    Nothing -> liftIO . putStrLn $ "There was no feed to write... todo make this a proper error"
    
  liftIO . BS.writeFile (siteDir <> "/robots.txt") $
    "User-agent: *\nAllow: /\nSitemap: " <> BS.pack (show sitemapUrl')
  make slug' (page (makeLinks Nothing (NE.trustedNonEmpty "#") (NE.trustedNonEmpty "All Posts") sortedPosts) (makeTags Nothing tags) renderedPosts) page404
