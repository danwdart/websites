{-# LANGUAGE UnicodeSyntax #-}

module Build.DanDart where

import           Control.Monad.IO.Class
import           Control.Monad.Trans.Reader
import           Data.Env.Types
import           Html.DanDart.Index
import           Make
import qualified Data.Text as T

build ∷ WebsiteIO ()
build = do
    slug' <- asks slug
    liftIO . putStrLn $ "Building " <> T.unpack slug'
    make slug' page page404

serve ∷ WebsiteIO ()
serve = asks slug >>= makeServe Build.DanDart.build
