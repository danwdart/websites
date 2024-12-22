{-# LANGUAGE DerivingVia     #-}
{-# LANGUAGE StrictData      #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedLists #-}

module Data.Env.Types where

import Control.Lens
import Control.Monad.Reader
import Data.ByteString.Char8 (ByteString)
import Data.List.NonEmpty qualified as LNE
import Data.List.NonEmpty (NonEmpty)
import Data.Set              (Set)
-- huh? NESet is not IsList? Well I suppose not... maybe there should be an IsNEList...
-- import Data.Set.NonEmpty     (NESet)
import Data.Text             (Text)
import Network.URI
import Text.Email.Parser

data SiteType = Normal | Blog {
    _atomTitle :: Text,
    _atomUrl   :: URI
}

makeLenses ''SiteType

data Urls = Urls {
    _urlDanDart      :: URI,
    _urlHamRadio     :: URI,
    _urlBlogHamRadio :: URI,
    _urlBlog         :: URI,
    _urlBlogJolHarg  :: URI,
    _urlJolHarg      :: URI,
    _urlMadHacker    :: URI
}

makeLenses ''Urls

-- not a map because ordered
newtype Breadcrumb = Breadcrumb {
    getBreadcrumb :: NonEmpty (Text, Maybe URI)
}

data Website = Website {
    _slug        :: Text,
    _title       :: Text,
    -- _keywords :: NESet Text,
    _description :: Text,
    _imgUrl      :: URI,
    _baseUrl     :: URI,
    _pageUrl     :: URI,
    _urls        :: Urls,
    _breadcrumb  :: Breadcrumb,
    _siteType    :: SiteType,
    _email       :: EmailAddress,
    _livereload  :: Bool,
    _build       :: ReaderT Website IO ()
}

makeLenses ''Website

-- TODO lens it?
plainBreadcrumb :: MonadReader Website m => Text -> m a -> m a
plainBreadcrumb breadcrumb' = local (\w -> w { _breadcrumb = Breadcrumb [(breadcrumb', Nothing)] })

addBreadcrumb :: MonadReader Website m => Text -> m a -> m a
addBreadcrumb breadcrumb' page = do
    breadcrumbExisting <- view breadcrumb
    baseUrl' <- view baseUrl
    let firstText = fst . LNE.head $ getBreadcrumb breadcrumbExisting
    local (\w -> w { _breadcrumb = Breadcrumb [(firstText, Just baseUrl'), (breadcrumb', Nothing)]}) page

instance Eq Website where
    Website {_slug = slug1} == Website {_slug = slug2} = slug1 == slug2

instance Ord Website where
    compare Website {_slug = slug1} Website {_slug = slug2} = compare slug1 slug2

type Env = Set Website
