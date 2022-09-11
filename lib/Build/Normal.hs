module Build.Normal where

import           Build.Sitemap
import           Control.Monad.IO.Class
import           Control.Monad.Reader
import qualified Data.ByteString.Lazy.Char8 as BSL
import           Data.Env.Types
import qualified Data.Text as T
import           Make
import           Text.Blaze.Html5 as H hiding (main)
import           Web.Sitemap.Gen

build ∷ WebsiteM Html -> WebsiteM Html -> WebsiteIO ()
build page page404 = do
    slug' <- asks slug
    sitemap' <- sitemap
    liftIO . BSL.writeFile ( ".sites/" <> T.unpack slug' <> "/sitemap.xml") $ renderSitemap sitemap'
    make slug' page page404