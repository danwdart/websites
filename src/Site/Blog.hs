{-# LANGUAGE DeriveGeneric, OverloadedStrings #-}
module Site.Blog where

import Build.Utils
import Cheapskate
import Control.Monad
import Data.Aeson (FromJSON, Object, (.:), (.:?))
import qualified Data.Aeson as A
import Data.Bifunctor
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as B
import Data.Frontmatter
import Data.List
import Data.Maybe
import Data.Ord
import Data.String
import Data.Text (Text)
import Data.Text.Encoding
import Data.Time
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import GHC.Generics
import Html.Blog.Index
import Network.Wai.Application.Static
import Network.Wai.Handler.Warp
import System.Directory
import System.FilePath
import Text.Blaze.Html5 as H hiding (main)
import Text.Blaze.Html5.Attributes as A
import Text.Blaze.Internal
import Text.Blaze.Renderer.Pretty
-- import Text.Blaze.Renderer.Utf8
import WaiAppStatic.Types
import Debug.Trace

newtype BlogTag = BlogTag {
    getTag :: Text
 } deriving (Show)

instance FromJSON BlogTag where
    parseJSON (A.String a) = return $ BlogTag a
    parseJSON (A.Number a) = return $ BlogTag $ T.pack (show a)
    parseJSON (A.Bool a) = return $ BlogTag $ T.pack (show a)
    parseJSON e = error (show e)

data BlogMetadata = BlogMetadata {
    title :: Text,
    date :: UTCTime,
    draft :: Bool,
    aliases :: [FilePath],
    tags :: [BlogTag] -- Doesn't like tags which are numbers... nor don't have tags
} deriving (Generic, Show)

data BlogComment = BlogComment {
    commentFrom :: Text,
    commentText :: Text,
    commentDate :: UTCTime
}

data BlogPost = BlogPost {
    metadata :: BlogMetadata,
    html :: Html,
    comments :: [Html]
}

data ParseResult = ParseResult {
    resultMetadata :: BlogMetadata,
    resultHtml :: Html
}

instance FromJSON BlogMetadata where
    parseJSON (A.Object o) = BlogMetadata <$>
        o .: "title" <*>
        o .: "date" <*>
        o .: "draft" <*>
        o .: "aliases" <*>
        (concat <$> (o .:? "tags")) -- Maybe [a] -> [a]

-- Remove <h4> blocks
cleanBlocks :: Block -> Block
cleanBlocks (Header 4 inlines) = HtmlBlock ""
cleanBlocks a = a

cleanDoc :: Doc -> Doc
cleanDoc (Doc opts blocks) = Doc opts $ cleanBlocks <$> blocks

parseFile :: Text -> ParseResult
parseFile contents = case parseYamlFrontmatter (encodeUtf8 contents) of
    Done i r -> ParseResult r $ toMarkup $ cleanDoc $ markdown def $ decodeUtf8 i
    Fail i xs y -> error $ "Failure of " ++ show xs ++ y
    _ -> error $ "What is " <> T.unpack contents

getCommentsIfExists :: FilePath -> IO [Html]
getCommentsIfExists postId = do
    commentFiles <- getDirectoryContents $ "posts/" <> postId
    let commentFileNames = ("posts/" </>) . (postId </>) <$> commentFiles
    validCommentFiles <- filterM doesFileExist commentFileNames
    commentTexts <- sequence $ TIO.readFile <$> validCommentFiles
    let commentData = parseFile <$> commentTexts
    return $ resultHtml <$> (
        sortOn (Down . date . resultMetadata) .
        filter (not . draft . resultMetadata) $
        commentData
        )

getComments :: FilePath -> IO [Html]
getComments postId = do
    dirExists <- doesDirectoryExist $ "posts/" <> postId
    if dirExists
        then getCommentsIfExists postId
        else return mempty

makeBlogPost :: FilePath -> IO BlogPost
makeBlogPost filename = do
    fileText <- TIO.readFile filename
    let (ParseResult metadata html) = parseFile fileText
    let postId = dropExtension $ takeFileName filename
    comments <- getComments postId
    return $ BlogPost metadata html comments

renderPost :: BlogPost -> Html
renderPost (BlogPost metadata html comments) = do
    let postId = dropExtension $ takeFileName $ Data.List.head $ aliases metadata
    a ! name (fromString postId) $ mempty
    h1 $ fromString $ T.unpack $ Site.Blog.title metadata
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
        mconcat comments
    br
    details $ do
        H.summary $ h4 ! A.class_ "d-inline-block" $ "Post a comment"
        H.form
            ! A.class_ "form"
            ! enctype "application/x-www-form-urlencoded"
            ! action "https://postb.in/1595622105966-7620481813792"
            ! method "post"
            ! target "_result" $ do
                H.input ! A.type_ "hidden" ! name "postId" ! value (fromString postId)
                mapM_ (\(type_, name_, label_, placeholder_) -> H.div ! A.class_ "form-group" $ do
                    H.label ! for name_ $ label_
                    H.input ! A.type_ type_ ! A.class_ "form-control" ! name name_ ! placeholder placeholder_
                    ) [
                        ("text", "name", "Name", "John Smith"),
                        ("email", "email", "Email", "john@smith.com"),
                        ("text", "website", "Website", "https://mydomain.com"),
                        ("text", "subject", "Subject", "My Subject")
                        ]
                H.div ! A.class_ "form-group" $ do
                    H.label ! for "name" $ "Comment"
                    H.textarea ! A.class_ "form-control" ! name "comment" ! placeholder "I think..." $ mempty
                H.div ! A.class_ "form-group" $ do
                    button ! A.type_ "submit" ! A.class_ "btn btn-primary" $ "Submit"
                    H.iframe ! name "_result" ! height "30" ! width "200" ! A.style "border: 0; vertical-align: middle; margin-left: 10px;" $ mempty
    hr
    

build :: IO ()
build = do
    files <- getDirectoryContents "posts"
    let fileNames = ("posts/" </>) <$> files
    validFiles <- filterM doesFileExist fileNames
    posts <- sequence $ makeBlogPost <$> validFiles
    let rendered = sortOn (Down . date . metadata) . filter (not . draft . metadata) $ posts
    let renderedPosts = foldMap renderPost rendered
    make "blog" $ page renderedPosts

serve :: IO ()
serve = do
    putStrLn "Building..."
    build
    putStrLn "Serving..."
    runEnv 80 . staticApp $ (defaultWebAppSettings ".sites/blog/"){ ssIndices = mapMaybe toPiece ["index.html"] } 