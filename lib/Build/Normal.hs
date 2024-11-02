module Build.Normal where

import Build.Sitemap
import Control.Monad.IO.Class
import Control.Monad.Reader
import Data.ByteString.Lazy.Char8 qualified as BSL
import Data.Env.Types
import Data.Text                  qualified as T
import Make
import Text.Blaze.Html5           as H hiding (main)
import Web.Sitemap.Gen

build ∷ (MonadReader Website m, MonadIO m) ⇒ m Html → m Html → m ()
build page page404 = do
    slug' <- asks slug
    sitemap' <- sitemap
    liftIO . BSL.writeFile ( ".sites/" <> T.unpack slug' <> "/sitemap.xml") $ renderSitemap sitemap'
    make slug' page page404
