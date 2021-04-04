{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE UnicodeSyntax     #-}

module Blog.Types where

import           Data.Aeson       (FromJSON, (.:), (.:?))
import qualified Data.Aeson       as A
import           Data.Text        (Text)
import qualified Data.Text        as T
import           Data.Time
import           GHC.Generics
import           Text.Blaze.Html5 as H hiding (main)

newtype BlogTag = BlogTag {
    getTag :: Text
 } deriving (Show)

data Score = Score {
    rating :: Int,
    outOf :: Int
}

instance FromJSON Score where
    parseJSON (A.String a') = do
        let [rating', outOf'] = T.splitOn "/" a'
        pure $ Score (read (T.unpack rating')) (read (T.unpack outOf'))
    parseJSON _ = fail "Problem parsing score."

instance FromJSON BlogTag where
    parseJSON (A.String a') = pure $ BlogTag a'
    parseJSON (A.Number a') = pure . BlogTag $ T.pack (show a')
    parseJSON (A.Bool a')   = pure . BlogTag $ T.pack (show a')
    parseJSON e             = fail (show e)

data BlogMetadata = BlogMetadata {
    title   :: Text,
    date    :: UTCTime,
    draft   :: Bool,
    aliases :: [FilePath], -- TODO make these files and use them for permalink?
    featuredImage :: Maybe Text,
    tags    :: [BlogTag], -- Doesn't like tags which are numbers... nor don't have tags
    scores  :: Maybe [(Text, Score)]
} deriving (Generic)

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
    parseJSON _ = fail "Bad blog comment metadata"

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
        o .:? "featuredImage" <*>
        (concat <$> (o .:? "tags")) <*> -- Maybe [a] -> [a]
        o .:? "scores"
    parseJSON _ = fail "Bad blog metadata"
