{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE StrictData  #-}

module Data.Env.Types where

import Control.Monad.Reader
import Data.ByteString.Char8 (ByteString)
import Data.Set              (Set)
import Data.Text             (Text)

data SiteType = Normal | Blog

type Url = ByteString

data Urls = Urls {
    urlDanDart      :: Url,
    urlHamRadio     :: Url,
    urlBlogHamRadio :: Url,
    urlBlog         :: Url,
    urlBlogJolHarg  :: Url,
    urlJolHarg      :: Url,
    urlMadHacker    :: Url
}

data Website = Website {
    slug        :: Text,
    title       :: Text,
    -- keywords :: Set Text,
    description :: Text,
    imgUrl      :: Url,
    url         :: Url,
    urls        :: Urls,
    siteType    :: SiteType,
    email       :: Text, -- TODO email
    livereload  :: Bool,
    build       :: ReaderT Website IO ()
}

instance Eq Website where
    Website {slug = slug1} == Website {slug = slug2} = slug1 == slug2

instance Ord Website where
    compare Website {slug = slug1} Website {slug = slug2} = compare slug1 slug2

type Env = Set Website
