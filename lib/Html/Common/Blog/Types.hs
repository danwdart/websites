{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData        #-}

module Html.Common.Blog.Types where

import Data.Aeson         (FromJSON, (.:))
import Data.Aeson         qualified as A
import Data.Aeson.Types   qualified as A
-- import Data.ByteString.Char8 qualified as BS
--- import Data.List.NonEmpty (NonEmpty)
import Data.List.NonEmpty as LNE
import Data.Text          (Text)
import Data.Text          qualified as T
import Data.Time
import GHC.Generics
import Text.Blaze.Html5   as H hiding (main)
import Text.Read

newtype BlogTag = BlogTag {
    getTag :: Text
 } deriving newtype (Show, Eq, Ord)

data Score = Score {
    rating :: Int,
    outOf  :: Int
}

instance FromJSON Score where
    parseJSON (A.String a') = do
        case T.splitOn "/" a' of
            [rating', outOf'] -> do
                case (do
                    rating'' <- readEither . T.unpack $ rating'
                    outOf'' <- readEither . T.unpack $ outOf'
                    pure $ Score rating'' outOf''
                    ) of
                    Left errMsg -> fail errMsg
                    Right score -> pure score
            _ -> fail "Problem parsing score."
    parseJSON invalid = A.typeMismatch "String" invalid

instance FromJSON BlogTag where
    parseJSON (A.String a') = pure $ BlogTag a'
    parseJSON (A.Number a') = pure . BlogTag $ T.show a'
    parseJSON (A.Bool a')   = pure . BlogTag $ T.show a'
    parseJSON other         = A.typeMismatch "String | Number | Bool" other

data BlogMetadata = BlogMetadata {
    title         :: Text,
    date          :: UTCTime,
    draft         :: Bool,
    aliases       :: NonEmpty FilePath,
    featuredImage :: Maybe Text,
    tags          :: NonEmpty BlogTag, -- Doesn't like tags which are numbers... nor don't have tags
    scores        :: Maybe (NonEmpty (Text, Score))
} deriving stock (Generic)
    deriving anyclass (FromJSON)

data BlogCommentMetadata = BlogCommentMetadata {
    author      :: Text,
    authorEmail :: Text,
    authorUrl   :: Maybe Text
} deriving stock (Generic, Show)

instance FromJSON BlogCommentMetadata where
    parseJSON (A.Object o) = BlogCommentMetadata <$>
        o .: "author" <*>
        o .: "email" <*>
        o .: "url"
    parseJSON other = A.typeMismatch "Object" other

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
