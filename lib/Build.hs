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
import qualified Data.Text as T

build ∷ WebsitesIO ()
build = do
    websites <- ask
    mapM_ (\website -> do
        liftIO . putStrLn $ "Running build on " <> T.unpack (Env.slug website)
        pure $ runReaderT (do
                liftIO . putStrLn $ "Env.build"
                Env.build website
            ) website
        ) websites

runBuild :: IO ()
runBuild = do
    putStrLn "Running build"
    runReaderT Build.build production