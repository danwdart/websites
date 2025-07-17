module Control.Exception.CommentException where

import Control.Exception
import Control.Exception.InvalidDateException
import Control.Exception.ParseFileException

-- we could probably do better here
data CommentException = CommentInvalidDateException InvalidDateException | CommentParseFileException ParseFileException
    deriving stock (Show)

instance Exception CommentException