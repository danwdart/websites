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
    endpoint :: Text
}

type Env = Map Text Website

type WebsiteM = Reader Website
type WebsiteT = ReaderT Website
type WebsiteIO = WebsiteT IO

type WebsitesM = Reader Env
type WebsitesT = ReaderT Env
type WebsitesIO = WebsitesT IO

dev :: Env
dev = []

--    endpoint = "http://localhost:3000/dev",
--    tracking = False,

live :: Env
live = []
--     endpoint = "https://kkeacv0mpj.execute-api.eu-west-2.amazonaws.com/dev",
--     tracking = True,
