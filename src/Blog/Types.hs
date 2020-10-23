{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

module Blog.Types where

import           Control.Applicative
import           Control.Monad
import           Data.Aeson                     (FromJSON, Object, (.:), (.:?))
import qualified Data.Aeson                     as A
import           Data.Bifunctor
import           Data.ByteString                (ByteString)
import qualified Data.ByteString.Char8          as B
import           Data.Foldable
import           Data.Function
import           Data.List
import           Data.Maybe
import           Data.Ord
import           Data.String
import           Data.Text                      (Text)
import qualified Data.Text                      as T
import           Data.Text.Encoding
import qualified Data.Text.IO                   as TIO
import           Data.Time
import           Data.Time.Format.ISO8601
import           GHC.Generics
import qualified Text.Atom.Feed as Atom
import qualified Text.Atom.Feed.Export as Export
import           Text.Blaze.Html5               as H hiding (main)
import           Text.Blaze.Html5.Attributes    as A
import           Text.Blaze.Internal
import           Text.Blaze.Renderer.Pretty
-- import Text.Blaze.Renderer.Utf8

newtype BlogTag = BlogTag {
    getTag :: Text
 } deriving (Show)

instance FromJSON BlogTag where
    parseJSON (A.String a) = return $ BlogTag a
    parseJSON (A.Number a) = return $ BlogTag $ T.pack (show a)
    parseJSON (A.Bool a)   = return $ BlogTag $ T.pack (show a)
    parseJSON e            = error (show e)

data BlogMetadata = BlogMetadata {
    title   :: Text,
    date    :: UTCTime,
    draft   :: Bool,
    aliases :: [FilePath],
    tags    :: [BlogTag] -- Doesn't like tags which are numbers... nor don't have tags
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

data BlogPost = BlogPost {
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
        (concat <$> (o .:? "tags")) -- Maybe [a] -> [a]