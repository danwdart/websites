{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE UnicodeSyntax     #-}

module Build.DanDart where

import           Control.Monad.IO.Class
import           Control.Monad.Reader
import qualified Data.ByteString.Lazy.Char8 as BSL
import           Data.Env.Types
import           Data.Time.Clock
import           Html.DanDart.Index
import           Make
import           Web.Sitemap.Gen

sitemap ∷ (MonadReader Website m, MonadIO m) => m Sitemap
sitemap = do
    now <- liftIO getCurrentTime
    pure $ Sitemap [
        SitemapUrl "https://dandart.co.uk/" (Just now) (Just Weekly) (Just 1.0)
        ]

build ∷ WebsiteIO ()
build = do
    slug' <- asks slug
    sitemap' <- sitemap
    liftIO . BSL.writeFile ".sites/dandart/sitemap.xml" $ renderSitemap sitemap'
    make slug' page page404

serve ∷ WebsiteIO ()
serve = asks slug >>= makeServe Build.DanDart.build
