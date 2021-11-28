{-# LANGUAGE DerivingVia       #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE StrictData        #-}
{-# LANGUAGE UnicodeSyntax     #-}

module Data.Env.Types where

import           Control.Applicative        (liftA2)
import           Control.Monad.Trans.Reader
import           Data.Functor.Identity
import           Data.Map                   (Map)
import           Data.String
import           Data.Text                  (Text)

newtype PostsLocation = PostsLocation {
    getPostsLocation :: Text
} deriving IsString via Text

data SiteType = Normal | Blog PostsLocation

data Urls = Urls {
    urlDanDart :: Text,
    urlHamRadio :: Text,
    urlBlog :: Text,
    urlJolHarg :: Text,
    urlMadHacker :: Text
}

data Website = Website {
    slug         :: Text,
    title        :: Text,
    -- keywords :: Set Text,
    url          :: Text,
    urls         :: Urls,
    siteType     :: SiteType,
    livereload   :: Bool,
    endpoint     :: Text,
    build        :: WebsiteIO (),
    serve        :: WebsiteIO ()
}

type Env = Map Text Website

type WebsiteM = Reader Website
type WebsiteT = ReaderT Website
type WebsiteIO = WebsiteT IO

instance (Semigroup a) => Semigroup (WebsiteM a) where
    (<>) = liftA2 (<>)

instance (Monoid a) => Monoid (WebsiteM a) where
    mempty = pure mempty
    mappend = liftA2 mappend

websiteMToWebsiteIO ∷ WebsiteM a → WebsiteIO a
websiteMToWebsiteIO = mapReaderT (pure . runIdentity)

type WebsitesM = Reader Env
type WebsitesT = ReaderT Env
type WebsitesIO = WebsitesT IO