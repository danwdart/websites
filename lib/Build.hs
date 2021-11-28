{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE UnicodeSyntax     #-}

module Build where
    
import           Control.Monad
import           Control.Monad.Trans.Reader
import           Data.Env as Env
import           Data.Env.Types as Env
import           Prelude                        hiding (putStrLn)

build ∷ WebsitesIO ()
build = do
    websites <- ask
    mapM_ (\website -> pure $ runReaderT (Env.build website) website) websites

runBuild :: IO ()
runBuild = runReaderT Build.build production