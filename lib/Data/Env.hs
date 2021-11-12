{-# LANGUAGE DerivingVia       #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedLists   #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData        #-}
{-# LANGUAGE UnicodeSyntax     #-}

module Data.Env where

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
    endpoint     :: Text
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

developmentUrls, productionUrls :: Urls
developmentUrls = Urls {
    urlDanDart = "http://dandart.localhost:8080",
    urlHamRadio = "http://m0ori.localhost:8080",
    urlBlog = "http://blog.localhost:8080",
    urlJolHarg = "http://jolharg.localhost:8080",
    urlMadHacker = "http://madhacker.localhost:8080"
}

productionUrls = Urls {
    urlDanDart = "https://dandart.co.uk",
    urlHamRadio = "https://m0ori.com",
    urlBlog = "https://blog.dandart.co.uk",
    urlJolHarg = "https://jolharg.com",
    urlMadHacker = "https://madhackerreviews.com"
}

development, production ∷ Env
development = [
    (
        "blog",
        Website {
            slug = "blog",
            title = "Dan Dart's Blog: Software Engineer, Mathematics Lover, Radio) Ham, Musician",
            url = "http://blog.localhost:8080",
            urls = developmentUrls,
            siteType = Blog "posts",
            livereload = False,
            endpoint = "http://localhost:3000/dev"
        }
    ),
    (
        "dandart",
        Website {
            slug = "dandart",
            title = "Dan Dart: Software Engineer, Mathematics Lover, Radio Ham, Musician",
            url = "http://dandart.localhost:8080",
            urls = developmentUrls,
            siteType = Normal,
            livereload = False,
            endpoint = "http://localhost:3000/dev"
        }
    ),
    (
        "jolharg",
        Website {
            slug = "jolharg",
            title = "JolHarg: Your Software Engineering Partner",
            url = "http://jolharg.localhost:8080",
            urls = developmentUrls,
            siteType = Normal,
            livereload = False,
            endpoint = "http://localhost:3000/dev"
        }
    ),
    (
        "m0ori",
        Website {
            slug = "m0ori",
            title = "M0ORI call sign: Dan Dart, England",
            url = "http://m0ori.localhost:8080",
            urls = developmentUrls,
            siteType = Normal,
            livereload = False,
            endpoint = "http://localhost:3000/dev"
        }
    ),
    (
        "madhacker",
        Website {
            slug = "madhacker",
            title = "The Mad Hacker: Tech Reviews by a crazy computer enthusiast",
            url = "http://madhacker.localhost:8080",
            urls = developmentUrls,
            siteType = Blog "reviews",
            livereload = False,
            endpoint = "http://localhost:3000/dev"
        }
    )
    ]

production = [
    (
        "blog",
        Website {
            slug = "blog",
            title = "Dan Dart's Blog: Software Engineer, Mathematics Lover, Radio Ham, Musician",
            url = "https://blog.dandart.co.uk",
            urls = productionUrls,
            siteType = Blog "posts",
            livereload = False,
            endpoint = "https://api.jolharg.com"
        }
    ),
    (
        "dandart",
        Website {
            slug = "dandart",
            title = "Dan Dart: Software Engineer, Mathematics Lover, Radio Ham, Musician",
            url = "https://dandart.co.uk",
            urls = productionUrls,
            siteType = Normal,
            livereload = False,
            endpoint = "https://api.jolharg,com"
        }
    ),
    (
        "jolharg",
        Website {
            slug = "jolharg",
            title = "JolHarg: Your Software Engineering Partner",
            url = "https://jolharg.com",
            urls = productionUrls,
            siteType = Normal,
            livereload = False,
            endpoint = "https://api.jolharg,com"
        }
    ),
    (
        "m0ori",
        Website {
            slug = "m0ori",
            title = "M0ORI call sign: Dan Dart, England",
            url = "https://m0ori.com",
            urls = productionUrls,
            siteType = Normal,
            livereload = False,
            endpoint = "https://api.jolharg,com"
        }
    ),
    (
        "madhacker",
        Website {
            slug = "madhacker",
            title = "The Mad Hacker: Tech Reviews by a crazy computer enthusiast",
            url = "https://madhackerreviews.com",
            urls = productionUrls,
            siteType = Blog "reviews",
            livereload = False,
            endpoint = "https://api.jolharg,com"
        }
    )
    ]
