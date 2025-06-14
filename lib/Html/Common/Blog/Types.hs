{-# LANGUAGE DerivingVia    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData        #-}

module Html.Common.Blog.Types where

import Data.Aeson         (FromJSON, (.:))
import Data.Aeson         qualified as A
import Data.Aeson.Types   qualified as A
import Data.Aeson.Types.Instances.NonEmpty ()
-- import Data.ByteString.Char8 qualified as BS
import Data.List.NonEmpty (NonEmpty)
import Data.NonEmpty      qualified as NE
import Data.Text.NonEmpty (NonEmptyText)
import Data.Text          (Text)
import Data.Text          qualified as T
import Data.Time
import GHC.Generics
import Network.URI
import Text.Blaze.Html5   as H hiding (main)
import Text.Read

newtype BlogTag = BlogTag {
    getTag :: NonEmptyText
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
    parseJSON (A.String a') = case NE.nonEmpty a' of
        Nothing -> fail "Empty string"
        Just a'' -> pure $ BlogTag a''
    parseJSON (A.Number a') = pure . BlogTag . NE.trustedNonEmpty . T.show $ a'
    parseJSON (A.Bool a')   = pure . BlogTag . NE.trustedNonEmpty . T.show $ a'
    parseJSON other         = A.typeMismatch "String | Number | Bool" other

data BlogMetadata = BlogMetadata {
    title         :: NonEmptyText,
    date          :: UTCTime,
    draft         :: Bool,
    aliases       :: NonEmpty FilePath,
    featuredImage :: Maybe URI,
    tags          :: NonEmpty BlogTag, -- Doesn't like tags which are numbers... nor don't have tags
    scores        :: Maybe (NonEmpty (Text, Score))
} deriving stock (Generic)
    deriving (FromJSON) via Generically BlogMetadata

data BlogCommentMetadata = BlogCommentMetadata {
    author      :: NonEmptyText,
    authorEmail :: NonEmptyText,
    authorUrl   :: Maybe URI
} deriving stock (Generic, Show)

instance FromJSON BlogCommentMetadata where
    parseJSON (A.Object o) = BlogCommentMetadata <$>
        o .: "author" <*>
        o .: "email" <*>
        o .: "url"
    parseJSON other = A.typeMismatch "Object" other

data BlogPost = BlogPost {
    postId   :: NonEmptyText,
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
