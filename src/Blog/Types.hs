{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE UnicodeSyntax     #-}

module Blog.Types where

import           Data.Aeson       (Value, FromJSON, (.:), (.:?))
import qualified Data.Aeson       as A
import           Data.Text        (Text)
import qualified Data.Text        as T
import           Data.Time
import           GHC.Generics
-- import qualified Text.Atom.Feed as Atom
-- import qualified Text.Atom.Feed.Export as Export
import           Text.Blaze.Html5 as H hiding (main)
-- import Text.Blaze.Renderer.Utf8

newtype BlogTag = BlogTag {
    getTag :: Text
 } deriving (Show)

instance FromJSON BlogTag where
    parseJSON (A.String a') = return $ BlogTag a'
    parseJSON (A.Number a') = return . BlogTag $ T.pack (show a')
    parseJSON (A.Bool a')   = return . BlogTag $ T.pack (show a')
    parseJSON e             = error (show e)

data BlogMetadata = BlogMetadata {
    title   :: Text,
    date    :: UTCTime,
    draft   :: Bool,
    aliases :: [FilePath],
    tags    :: [BlogTag], -- Doesn't like tags which are numbers... nor don't have tags
    scores  :: Maybe Value
} deriving (Generic, Show)

data BlogCommentMetadata = BlogCommentMetadata {
    author      :: Text,
    authorEmail :: Text,
    authorUrl   :: Maybe Text
} deriving (Generic, Show)

instance FromJSON BlogCommentMetadata where
    parseJSON (A.Object o) = BlogCommentMetadata <$>
        o .: "author" <*>
        o .: "email" <*>
        o .: "url"
    parseJSON _ = error "Bad blog comment metadata"

data BlogPost = BlogPost {
    postId   :: Text,
    metadata :: BlogMetadata,
    html     :: Html,
    comments :: [ParseCommentResult]
}

data ParseResult = ParseResult {
    resultMetadata :: BlogMetadata,
    resultHtml     :: Html
}

data ParseCommentResult = ParseCommentResult {
    commentDate     :: UTCTime,
    commentMetadata :: BlogCommentMetadata,
    commentHtml     :: Html
}

instance FromJSON BlogMetadata where
    parseJSON (A.Object o) = BlogMetadata <$>
        o .: "title" <*>
        o .: "date" <*>
        o .: "draft" <*>
        o .: "aliases" <*>
        (concat <$> (o .:? "tags")) <*> -- Maybe [a] -> [a]
        o .:? "scores"
    parseJSON _ = error "Bad blog metadata"
