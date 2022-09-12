{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE StrictData  #-}

module Data.Env.Types where

import           Control.Applicative   (liftA2)
import           Control.Monad.Reader
import           Data.Functor.Identity
import           Data.Set              (Set)
import           Data.Text             (Text)

data SiteType = Normal | Blog

data Urls = Urls {
    urlDanDart     :: Text,
    urlHamRadio    :: Text,
    urlBlog        :: Text,
    urlBlogJolHarg :: Text,
    urlJolHarg     :: Text,
    urlMadHacker   :: Text
}

data Website = Website {
    slug       :: Text,
    title      :: Text,
    -- keywords :: Set Text,
    url        :: Text,
    urls       :: Urls,
    siteType   :: SiteType,
    email      :: Text,
    livereload :: Bool,
    build      :: WebsiteIO ()
}

instance Eq Website where
    Website {slug = slug1} == Website {slug = slug2} = slug1 == slug2

instance Ord Website where
    compare Website {slug = slug1} Website {slug = slug2} = compare slug1 slug2

type Env = Set Website

type WebsiteM = Reader Website
type WebsiteT = ReaderT Website
type WebsiteIO = WebsiteT IO

instance (Semigroup a) ⇒ Semigroup (WebsiteM a) where
    (<>) = liftA2 (<>)

instance (Monoid a) ⇒ Monoid (WebsiteM a) where
    mempty = pure mempty
    mappend = (<>)

websiteMToWebsiteIO ∷ WebsiteM a → WebsiteIO a
websiteMToWebsiteIO = mapReaderT (pure . runIdentity)

type WebsitesM = Reader Env
type WebsitesT = ReaderT Env
type WebsitesIO = WebsitesT IO
