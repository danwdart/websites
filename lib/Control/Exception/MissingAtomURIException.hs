module Control.Exception.MissingAtomURIException where

import Control.Exception

data MissingAtomURIException = MissingAtomURIException
    deriving stock (Show)

instance Exception MissingAtomURIException where
    displayException _ = "Missing Atom URI for this supposed blog."