{-# LANGUAGE OverloadedStrings #-}

module Build.Normal where

import Build.Sitemap
import Control.Lens
import Control.Monad.IO.Class
import Control.Monad.Reader
import Data.ByteString.Char8  qualified as BS
import Data.Env.Types         as Env
import Data.NonEmpty          qualified as NE
import Data.Text              qualified as T
import Make
import Text.Blaze.Html5       as H hiding (main)
import Web.Sitemap.Gen

build ∷ (MonadReader Website m, MonadIO m) ⇒ m Html → m Html → m ()
build page page404 = do
    slug' <- view slug
    sitemapUrl' <- view sitemapUrl
    sitemap' <- sitemap
    liftIO . BS.writeFile ( ".sites/" <> T.unpack (NE.getNonEmpty slug') <> "/sitemap.xml") $ renderSitemap sitemap'
    liftIO . BS.writeFile ( ".sites/" <> T.unpack (NE.getNonEmpty slug') <> "/robots.txt") $ "User-agent: *\nAllow: /\nSitemap: " <> BS.pack (show sitemapUrl')
    make slug' page page404
