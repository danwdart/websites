{-# LANGUAGE DerivingVia     #-}
{-# LANGUAGE StrictData      #-}
{-# LANGUAGE TemplateHaskell #-}

module Data.Env.Types where

import Control.Lens
import Control.Monad.Reader
import Data.ByteString.Char8 (ByteString)
import Data.Set              (Set)
-- huh? NESet is not IsList? Well I suppose not... maybe there should be an IsNEList...
-- import Data.Set.NonEmpty     (NESet)
import Data.Text             (Text)

type Url = ByteString

data SiteType = Normal | Blog {
    _atomTitle :: Text,
    _atomUrl   :: Url
}

makeLenses ''SiteType

data Urls = Urls {
    _urlDanDart      :: Url,
    _urlHamRadio     :: Url,
    _urlBlogHamRadio :: Url,
    _urlBlog         :: Url,
    _urlBlogJolHarg  :: Url,
    _urlJolHarg      :: Url,
    _urlMadHacker    :: Url
}

makeLenses ''Urls

data Website = Website {
    _slug        :: Text,
    _title       :: Text,
    -- _keywords :: NESet Text,
    _description :: Text,
    _imgUrl      :: Url,
    _baseUrl     :: Url,
    _pageUrl     :: Url,
    _urls        :: Urls,
    _siteType    :: SiteType,
    _email       :: Text, -- TODO email
    _livereload  :: Bool,
    _build       :: ReaderT Website IO ()
}

makeLenses ''Website

instance Eq Website where
    Website {_slug = slug1} == Website {_slug = slug2} = slug1 == slug2

instance Ord Website where
    compare Website {_slug = slug1} Website {_slug = slug2} = compare slug1 slug2

type Env = Set Website
