{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE StrictData  #-}

module Data.Env.Types where

import Control.Monad.Reader
import Data.Set             (Set)
import Data.Text            (Text)

data SiteType = Normal | Blog

data Urls = Urls {
    urlDanDart      :: Text,
    urlHamRadio     :: Text,
    urlBlogHamRadio :: Text,
    urlBlog         :: Text,
    urlBlogJolHarg  :: Text,
    urlJolHarg      :: Text,
    urlMadHacker    :: Text
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
    build      :: ReaderT Website IO ()
}

instance Eq Website where
    Website {slug = slug1} == Website {slug = slug2} = slug1 == slug2

instance Ord Website where
    compare Website {slug = slug1} Website {slug = slug2} = compare slug1 slug2

type Env = Set Website
