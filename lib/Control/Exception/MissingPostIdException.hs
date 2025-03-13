module Control.Exception.MissingPostIdException where

import Control.Exception

data MissingPostIdException = MissingPostIdException
    deriving stock (Show)

instance Exception MissingPostIdException where
    displayException _ = "Missing post ID for this post."