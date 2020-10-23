{-# LANGUAGE OverloadedStrings #-}

module Blog.Post where

import           Blog.Comment
import           Blog.Types
import           Build.Utils
import           Cheapskate
import           Control.Applicative
import           Control.Monad
import           Data.Aeson                     (FromJSON, Object, (.:), (.:?))
import qualified Data.Aeson                     as A
import           Data.Bifunctor
import           Data.ByteString                (ByteString)
import qualified Data.ByteString.Char8          as B
import           Data.Foldable
import           Data.Frontmatter
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
import           Html.Blog.Index
import           Network.Wai.Application.Static
import           Network.Wai.Handler.Warp
import           System.Directory
import           System.FilePath
import qualified Text.Atom.Feed as Atom
import qualified Text.Atom.Feed.Export as Export
import           Text.Blaze.Html5               as H hiding (main)
import           Text.Blaze.Html5.Attributes    as A
import           Text.Blaze.Internal
import           Text.Blaze.Renderer.Pretty
-- import Text.Blaze.Renderer.Utf8
import           Text.XML
import           Util.Triple
import           WaiAppStatic.Types

parseFile :: Text -> ParseResult
parseFile contents = case parseYamlFrontmatter (encodeUtf8 contents) of
    Done i r    -> ParseResult r $ toMarkup $ markdown def $ decodeUtf8 i
    Fail i xs y -> error $ "Failure of " ++ show xs ++ y
    _           -> error $ "What is " <> T.unpack contents


makeBlogPost :: FilePath -> IO BlogPost
makeBlogPost filename = do
    fileText <- TIO.readFile filename
    let (ParseResult metadata html) = parseFile fileText
    let postId = dropExtension $ takeFileName filename
    comments <- getComments postId
    return $ BlogPost metadata html comments


getPostId :: BlogMetadata -> FilePath
getPostId = dropExtension . takeFileName . Data.List.head . aliases

renderPost :: BlogPost -> Html
renderPost (BlogPost metadata html comments) = do
    let postId = getPostId metadata
    a ! name (fromString postId) $ mempty
    -- Not working in Safari yet, so filter
    img ! height "0" ! width "0" ! src ("/favicon.ico?" <> fromString postId) ! customAttribute "loading" "lazy"
    h1 $ fromString $ T.unpack $ Blog.Types.title metadata
    small $ do
        a ! href ("#" <> fromString postId) $ "Permalink"
        " | Published: "
        fromString $ show $ date metadata
        " | Tags: "
        foldMap ((\str -> do
            a ! href "#" {-( <> fromString str)-} $ fromString str
            " "
            ) . T.unpack . getTag) (tags metadata)
    br
    br
    html
    br
    h3 "Comments"
    if Data.List.null comments then
        p "No comments at the moment. Be the first to comment!"
    else
        mapM_ renderComment comments
    br
    details $ do
        H.summary $ h4 ! A.class_ "d-inline-block" $ "Post a comment"
        commentForm postId
    hr