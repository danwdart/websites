module Control.Exception.BlogPostException where

import Control.Exception
import Control.Exception.CommentException
import Control.Exception.InvalidDateException
import Control.Exception.MissingPostIdException
import Control.Exception.ParseFileException

-- we could probably do better here
data BlogPostException = BlogPostCommentException CommentException
                        | BlogPostInvalidDateException InvalidDateException
                        | BlogPostMissingPostIdException MissingPostIdException
                        | BlogPostParseFileException ParseFileException
    deriving stock (Show)

instance Exception BlogPostException