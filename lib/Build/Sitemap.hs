module Build.Sitemap where

import Control.Lens
import Control.Monad.IO.Class
import Control.Monad.Reader
import Data.Env.Types
import Data.Text qualified as T
import Data.Time.Clock
import Web.Sitemap.Gen

sitemap ∷ (MonadReader Website m, MonadIO m) ⇒ m Sitemap
sitemap = do
    now <- liftIO getCurrentTime
    baseUrl' <- view baseUrl
    pure $ Sitemap [
        SitemapUrl (T.pack . show $ baseUrl') (Just now) (Just Weekly) (Just 1.0)
        ]
