{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE UnicodeSyntax     #-}

module Site.Build where

import           Control.Monad.IO.Class
import           Control.Monad.Trans.Reader
import           Data.Env
import           Data.Map                   ((!))
import           Prelude                    hiding (putStrLn)
import qualified Site.Blog                  as B
import qualified Site.DanDart               as D
import qualified Site.JolHarg               as J
import qualified Site.M0ORI                 as M
import qualified Site.MadHacker             as R

build ∷ WebsitesIO ()
build = do
    websites <- ask
    liftIO $ do
        runReaderT B.build $ websites ! "blog"
        runReaderT D.build $ websites ! "dandart"
        runReaderT J.build $ websites ! "jolharg"
        runReaderT M.build $ websites ! "m0ori"
        runReaderT R.build $ websites ! "madhacker"
