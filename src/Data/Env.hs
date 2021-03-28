{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}

module Data.Env where

import Control.Monad.Trans.Reader
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
    siteType :: SiteType,
    livereload :: Bool,
    tracking :: Bool,
    endpoint :: Text,
    dev :: Bool -- temporary
}

type Env = Map Text Website

-- mapReaderT

type WebsiteM = Reader Website
type WebsiteT = ReaderT Website
type WebsiteIO = WebsiteT IO

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
            url = "http://blog.localhost:8000",
            siteType = Blog "posts",
            livereload = True,
            tracking = False,
            endpoint = "http://localhost:3000/dev",
            dev = True
        }
    ),
    (
        "dandart",
        Website {
            slug = "dandart",
            title = "Dan Dart: Software Engineer, Mathematics Lover, Radio Ham, Musician",
            url = "http://dandart.localhost:8000",
            siteType = Normal,
            livereload = True,
            tracking = False,
            endpoint = "http://localhost:3000/dev",
            dev = True
        }
    ),
    (
        "jolharg",
        Website {
            slug = "jolharg",
            title = "JolHarg: Your Software Engineering Partner",
            url = "http://jolharg.localhost:8000",
            siteType = Normal,
            livereload = True,
            tracking = False,
            endpoint = "http://localhost:3000/dev",
            dev = True
        }
    ),
    (
        "m0ori",
        Website {
            slug = "m0ori",
            title = "M0ORI call sign: Dan Dart, England",
            url = "http://m0ori.localhost:8000",
            siteType = Normal,
            livereload = True,
            tracking = False,
            endpoint = "http://localhost:3000/dev",
            dev = True
        }
    ),
    (
        "madhacker",
        Website {
            slug = "m0ori",
            title = "The Mad Hacker: Tech Reviews by a crazy computer enthusiast",
            url = "http://madhacker.localhost:8000",
            siteType = Blog "reviews",
            livereload = True,
            tracking = False,
            endpoint = "http://localhost:3000/dev",
            dev = True
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
            siteType = Blog "posts",
            livereload = True,
            tracking = False,
            endpoint = "https://kkeacv0mpj.execute-api.eu-west-2.amazonaws.com/dev",
            dev = False
        }
    ),
    (
        "dandart",
        Website {
            slug = "dandart",
            title = "Dan Dart: Software Engineer, Mathematics Lover, Radio Ham, Musician",
            url = "https://dandart.co.uk",
            siteType = Normal,
            livereload = True,
            tracking = False,
            endpoint = "https://kkeacv0mpj.execute-api.eu-west-2.amazonaws.com/dev",
            dev = False
        }
    ),
    (
        "jolharg",
        Website {
            slug = "jolharg",
            title = "JolHarg: Your Software Engineering Partner",
            url = "https://jolharg.com",
            siteType = Normal,
            livereload = True,
            tracking = False,
            endpoint = "https://kkeacv0mpj.execute-api.eu-west-2.amazonaws.com/dev",
            dev = False
        }
    ),
    (
        "m0ori",
        Website {
            slug = "m0ori",
            title = "M0ORI call sign: Dan Dart, England",
            url = "https://m0ori.co.uk",
            siteType = Normal,
            livereload = True,
            tracking = False,
            endpoint = "https://kkeacv0mpj.execute-api.eu-west-2.amazonaws.com/dev",
            dev = False
        }
    ),
    (
        "madhacker",
        Website {
            slug = "m0ori",
            title = "The Mad Hacker: Tech Reviews by a crazy computer enthusiast",
            url = "https://madhackerreviews.com",
            siteType = Blog "reviews",
            livereload = True,
            tracking = False,
            endpoint = "https://kkeacv0mpj.execute-api.eu-west-2.amazonaws.com/dev",
            dev = False
        }
    )
    ]