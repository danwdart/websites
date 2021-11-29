{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE UnicodeSyntax     #-}

module Build where
    
import           Control.Monad
import           Control.Monad.IO.Class
import           Control.Monad.Trans.Reader
import           Data.Env as Env
import           Data.Env.Types as Env
import           Prelude

build ∷ WebsitesIO ()
build = ask >>= mapM_ (\website -> liftIO $ runReaderT (Env.build website) website)

runBuild :: IO ()
runBuild = runReaderT Build.build production