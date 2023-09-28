module Build.Sitemap where

import Control.Monad.IO.Class
import Control.Monad.Reader
import Data.Env.Types
import Data.Time.Clock
import Web.Sitemap.Gen

sitemap ∷ (MonadReader Website m, MonadIO m) ⇒ m Sitemap
sitemap = do
    now <- liftIO getCurrentTime
    url' <- asks url
    pure $ Sitemap [
        SitemapUrl url' (Just now) (Just Weekly) (Just 1.0)
        ]
