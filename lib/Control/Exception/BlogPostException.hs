module Control.Exception.BlogPostException where

import Control.Exception
import Control.Exception.CommentException
import Control.Exception.InvalidDateException
import Control.Exception.MissingPostIdException
import Control.Exception.ParseFileException

-- we could probably do better here
data BlogPostException = BPCEx CommentException | BPIDEx InvalidDateException | BPMPIEx MissingPostIdException | BPPFEx ParseFileException
    deriving stock (Show)

instance Exception BlogPostException