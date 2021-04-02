{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Data.Env where

import Control.Applicative
import Control.Monad.Trans.Reader
import Data.Functor.Identity
-- import Data.Set (Set)
import Data.String
import Data.Map (Map)
import Data.Text (Text)

newtype PostsLocation = PostsLocation {
    getPostsLocation :: Text
} deriving IsString via Text

data SiteType = Normal | Blog PostsLocation

data Website = Website {
    slug :: Text,
    title :: Text,
    -- keywords :: Set Text,
    url :: Text,
    urlDanDart :: Text,
    urlHamRadio :: Text,
    urlBlog :: Text,
    urlJolHarg :: Text,
    urlMadHacker :: Text,
    siteType :: SiteType,
    livereload :: Bool,
    tracking :: Bool,
    endpoint :: Text
}

type Env = Map Text Website

-- mapReaderT

type WebsiteM = Reader Website
type WebsiteT = ReaderT Website
type WebsiteIO = WebsiteT IO

websiteMToWebsiteIO :: WebsiteM a -> WebsiteIO a
websiteMToWebsiteIO = mapReaderT (pure . runIdentity)

-- todo Ap
instance (Applicative f, Semigroup m) => Semigroup (ReaderT r f m) where
    (<>) = liftA2 (<>)

instance (Applicative f, Monoid m) => Monoid (ReaderT r f m) where
    mempty = pure mempty
    mappend = liftA2 mappend

type WebsitesM = Reader Env
type WebsitesT = ReaderT Env
type WebsitesIO = WebsitesT IO

development :: Env
development = [
    (
        "blog",
        Website {
            slug = "blog",
            title = "Dan Dart's Blog: Software Engineer, Mathematics Lover, Radio Ham, Musician",
            url = "http://blog.localhost:8080",
            urlDanDart = "http://dandart.localhost:8080",
            urlHamRadio = "http://m0ori.localhost:8080",
            urlBlog = "http://blog.localhost:8080",
            urlJolHarg = "http://jolharg.localhost:8080",
            urlMadHacker = "http://madhacker.localhost:8080",
            siteType = Blog "posts",
            livereload = False,
            tracking = False,
            endpoint = "http://localhost:3000/dev"
        }
    ),
    (
        "dandart",
        Website {
            slug = "dandart",
            title = "Dan Dart: Software Engineer, Mathematics Lover, Radio Ham, Musician",
            url = "http://dandart.localhost:8080",
            urlDanDart = "http://dandart.localhost:8080",
            urlHamRadio = "http://m0ori.localhost:8080",
            urlBlog = "http://blog.localhost:8080",
            urlJolHarg = "http://jolharg.localhost:8080",
            urlMadHacker = "http://madhacker.localhost:8080",
            siteType = Normal,
            livereload = False,
            tracking = False,
            endpoint = "http://localhost:3000/dev"
        }
    ),
    (
        "jolharg",
        Website {
            slug = "jolharg",
            title = "JolHarg: Your Software Engineering Partner",
            url = "http://jolharg.localhost:8080",
            urlDanDart = "http://dandart.localhost:8080",
            urlHamRadio = "http://m0ori.localhost:8080",
            urlBlog = "http://blog.localhost:8080",
            urlJolHarg = "http://jolharg.localhost:8080",
            urlMadHacker = "http://madhacker.localhost:8080",
            siteType = Normal,
            livereload = False,
            tracking = False,
            endpoint = "http://localhost:3000/dev"
        }
    ),
    (
        "m0ori",
        Website {
            slug = "m0ori",
            title = "M0ORI call sign: Dan Dart, England",
            url = "http://m0ori.localhost:8080",
            urlDanDart = "http://dandart.localhost:8080",
            urlHamRadio = "http://m0ori.localhost:8080",
            urlBlog = "http://blog.localhost:8080",
            urlJolHarg = "http://jolharg.localhost:8080",
            urlMadHacker = "http://madhacker.localhost:8080",
            siteType = Normal,
            livereload = False,
            tracking = False,
            endpoint = "http://localhost:3000/dev"
        }
    ),
    (
        "madhacker",
        Website {
            slug = "m0ori",
            title = "The Mad Hacker: Tech Reviews by a crazy computer enthusiast",
            url = "http://madhacker.localhost:8080",
            urlDanDart = "http://dandart.localhost:8080",
            urlHamRadio = "http://m0ori.localhost:8080",
            urlBlog = "http://blog.localhost:8080",
            urlJolHarg = "http://jolharg.localhost:8080",
            urlMadHacker = "http://madhacker.localhost:8080",
            siteType = Blog "reviews",
            livereload = False,
            tracking = False,
            endpoint = "http://localhost:3000/dev"
        }
    )
    ]

production :: Env
production = [
    (
        "blog",
        Website {
            slug = "blog",
            title = "Dan Dart's Blog: Software Engineer, Mathematics Lover, Radio Ham, Musician",
            url = "https://blog.dandart.co.uk",
            urlDanDart = "https://dandart.co.uk",
            urlHamRadio = "https://m0ori.com",
            urlBlog = "https://blog.dandart.co.uk",
            urlJolHarg = "https://jolharg.com",
            urlMadHacker = "https://madhackerreviews.com",
            siteType = Blog "posts",
            livereload = False,
            tracking = True,
            endpoint = "https://kkeacv0mpj.execute-api.eu-west-2.amazonaws.com/dev"
        }
    ),
    (
        "dandart",
        Website {
            slug = "dandart",
            title = "Dan Dart: Software Engineer, Mathematics Lover, Radio Ham, Musician",
            url = "https://dandart.co.uk",
            urlDanDart = "https://dandart.co.uk",
            urlHamRadio = "https://m0ori.com",
            urlBlog = "https://blog.dandart.co.uk",
            urlJolHarg = "https://jolharg.com",
            urlMadHacker = "https://madhackerreviews.com",
            siteType = Normal,
            livereload = False,
            tracking = True,
            endpoint = "https://kkeacv0mpj.execute-api.eu-west-2.amazonaws.com/dev"
        }
    ),
    (
        "jolharg",
        Website {
            slug = "jolharg",
            title = "JolHarg: Your Software Engineering Partner",
            url = "https://jolharg.com",
            urlDanDart = "https://dandart.co.uk",
            urlHamRadio = "https://m0ori.com",
            urlBlog = "https://blog.dandart.co.uk",
            urlJolHarg = "https://jolharg.com",
            urlMadHacker = "https://madhackerreviews.com",
            siteType = Normal,
            livereload = False,
            tracking = True,
            endpoint = "https://kkeacv0mpj.execute-api.eu-west-2.amazonaws.com/dev"
        }
    ),
    (
        "m0ori",
        Website {
            slug = "m0ori",
            title = "M0ORI call sign: Dan Dart, England",
            url = "https://m0ori.com",
            urlDanDart = "https://dandart.co.uk",
            urlHamRadio = "https://m0ori.com",
            urlBlog = "https://blog.dandart.co.uk",
            urlJolHarg = "https://jolharg.com",
            urlMadHacker = "https://madhackerreviews.com",
            siteType = Normal,
            livereload = False,
            tracking = True,
            endpoint = "https://kkeacv0mpj.execute-api.eu-west-2.amazonaws.com/dev"
        }
    ),
    (
        "madhacker",
        Website {
            slug = "m0ori",
            title = "The Mad Hacker: Tech Reviews by a crazy computer enthusiast",
            url = "https://madhackerreviews.com",
            urlDanDart = "https://dandart.co.uk",
            urlHamRadio = "https://m0ori.com",
            urlBlog = "https://blog.dandart.co.uk",
            urlJolHarg = "https://jolharg.com",
            urlMadHacker = "https://madhackerreviews.com",
            siteType = Blog "reviews",
            livereload = False,
            tracking = True,
            endpoint = "https://kkeacv0mpj.execute-api.eu-west-2.amazonaws.com/dev"
        }
    )
    ]