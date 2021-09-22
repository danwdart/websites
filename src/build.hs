{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE UnicodeSyntax     #-}

module Main where

import           Control.Monad.Trans.Reader
import           Data.Env
import           Prelude                    hiding (putStrLn)
import           Site.Build

main ∷ IO ()
main = runReaderT build production
