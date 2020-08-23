{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}
module Site.Blog where

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
import           Text.Blaze.Html5               as H hiding (main)
import           Text.Blaze.Html5.Attributes    as A
import           Text.Blaze.Internal
import           Text.Blaze.Renderer.Pretty
-- import Text.Blaze.Renderer.Utf8
import           Debug.Trace
import           WaiAppStatic.Types

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

parseFile :: Text -> ParseResult
parseFile contents = case parseYamlFrontmatter (encodeUtf8 contents) of
    Done i r    -> ParseResult r $ toMarkup $ markdown def $ decodeUtf8 i
    Fail i xs y -> error $ "Failure of " ++ show xs ++ y
    _           -> error $ "What is " <> T.unpack contents

stringToTime :: String -> UTCTime
stringToTime s = fromJust (
    (zonedTimeToUTC <$> (iso8601ParseM s :: Maybe ZonedTime)) <|>
    iso8601ParseM s
    )

t1 :: (a, b, c) -> a
t1 (a, b, c) = a

t2 :: (a, b, c) -> b
t2 (a, b, c) = b

t3 :: (a, b, c) -> c
t3 (a, b, c) = c

year :: UTCTime -> Integer
year = t1 . toGregorian . utctDay

month :: UTCTime -> Int
month = t2 . toGregorian . utctDay

groupOn :: Eq a1 => (a2 -> a1) -> [a2] -> [[a2]]
groupOn = groupBy . on (==)

renderMetaLink :: BlogMetadata -> Html
renderMetaLink m = a ! href (fromString ("#" <> getPostId m)) $ fromString (T.unpack (Site.Blog.title m))

renderLink :: BlogPost -> Html
renderLink = renderMetaLink . metadata

makeLinks :: [BlogPost] -> Html
makeLinks = foldMap ((
        \byYear -> do
            details ! customAttribute "open" "" ! class_ "pl-2" $ do
                H.summary $ fromString . show . year . date . metadata . Data.List.head . Data.List.head $ byYear
                p $ foldMap (
                    \byMonth -> details! customAttribute "open" "" ! class_ "pl-2" $ do
                        -- "%B" is Month
                        H.summary $ fromString . formatTime defaultTimeLocale "%B" . date . metadata . Data.List.head $ byMonth
                        p $ foldMap (\link -> do
                            p ! class_ "pl-2" $ renderLink link
                            br
                            ) byMonth
                    ) byYear
        ) .
        groupOn (month . date . metadata)
    ) . groupOn (year . date . metadata)

{-
{-}
details $ do
    H.summary "2020"
    details  $ do
        H.summary "08"
        a ! class_ "pl-2" ! href "#" $ "The thing." -}
-}

parseComment :: UTCTime -> Text -> ParseCommentResult
parseComment date contents = case parseYamlFrontmatter (encodeUtf8 contents) of
    Done i r -> ParseCommentResult date r (toMarkup $ markdown def $ decodeUtf8 i)
    Fail i xs y -> error $ "Failure of " ++ show xs ++ y
    _ -> error $ "What is " <> T.unpack contents

getCommentsIfExists :: FilePath -> IO [ParseCommentResult]
getCommentsIfExists postId = do
    commentFiles <- getDirectoryContents $ "posts/" <> postId
    let commentFileNames = ("posts/" </>) . (postId </>) <$> commentFiles
    validCommentFiles <- filterM doesFileExist commentFileNames
    let dates = stringToTime . dropExtension . takeFileName <$> validCommentFiles
    commentTexts <- sequence $ TIO.readFile <$> validCommentFiles
    let commentData = zipWith parseComment dates commentTexts
    return $ sortOn (Down . commentDate) commentData

getComments :: FilePath -> IO [ParseCommentResult]
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

commentForm :: FilePath -> Html
commentForm postId = H.form
    ! A.class_ "form"
    ! enctype "application/x-www-form-urlencoded"
    ! action "https://kkeacv0mpj.execute-api.eu-west-2.amazonaws.com/dev/comment"
    ! method "post"
    ! target "_result" $ do
        H.input ! A.type_ "hidden" ! name "postId" ! value (fromString postId)
        mapM_ (\(type_, name_, label_, placeholder_) -> H.div ! A.class_ "form-group" $ do
            H.label ! for name_ $ label_
            H.input ! A.type_ type_ ! A.class_ "form-control" ! name name_ ! placeholder placeholder_
            ) [
                ("text", "name", "Name", "John Smith"),
                ("email", "email", "Email", "john@smith.com"),
                ("text", "website", "Website", "https://mydomain.com")
                ]
        H.div ! A.class_ "form-group" $ do
            H.label ! for "name" $ "Comment"
            H.textarea ! A.class_ "form-control" ! name "comment" ! placeholder "I think..." $ mempty
        H.div ! A.class_ "form-group" $ do
            button ! A.type_ "submit" ! A.class_ "btn btn-primary" $ "Submit"
            H.iframe ! name "_result" ! height "30" ! width "200" ! A.style "border: 0; vertical-align: middle; margin-left: 10px;" $ mempty

renderComment :: ParseCommentResult -> Html
renderComment ParseCommentResult {
    commentDate,
    commentMetadata = BlogCommentMetadata {
        author,
        authorEmail,
        authorUrl
    },
    commentHtml
    } = do
        small $ do
            a ! name (fromString (iso8601Show commentDate)) $ mempty
            a ! href ("mailto:" <> (fromString . T.unpack $ authorEmail)) $ string (T.unpack author)
            " "
            when (isJust authorUrl) $
                a ! href (fromString $ T.unpack $ fromJust authorUrl) $ " (URL)"
            " said on "
            a ! href ("#" <> fromString (iso8601Show commentDate)) $ fromString $ iso8601Show commentDate
            ":"
        p commentHtml
        br

getPostId :: BlogMetadata -> FilePath
getPostId = dropExtension . takeFileName . Data.List.head . aliases

renderPost :: BlogPost -> Html
renderPost (BlogPost metadata html comments) = do
    let postId = getPostId metadata
    a ! name (fromString postId) $ mempty
    -- Not working in Safari yet, so filter
    img ! height "0" ! width "0" ! src ("/favicon.ico?" <> fromString postId) ! customAttribute "loading" "lazy"
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
        mapM_ renderComment comments
    br
    details $ do
        H.summary $ h4 ! A.class_ "d-inline-block" $ "Post a comment"
        commentForm postId
    hr

build :: IO ()
build = do
    files <- getDirectoryContents "posts"
    let fileNames = ("posts/" </>) <$> files -- if used in same line, use Compose
    validFiles <- filterM doesFileExist fileNames
    posts <- sequence $ makeBlogPost <$> validFiles
    let rendered = sortOn (Down . date . metadata) . filter (not . draft . metadata) $ posts
    let renderedPosts = foldMap renderPost rendered
    let renderedLinks = makeLinks rendered
    make "blog" $ page renderedLinks renderedPosts

serve :: IO ()
serve = do
    putStrLn "Building..."
    build
    putStrLn "Serving..."
    runEnv 80 . staticApp $ (defaultWebAppSettings ".sites/blog/"){ ssIndices = mapMaybe toPiece ["index.html"] }
