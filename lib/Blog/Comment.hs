
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE UnicodeSyntax     #-}

module Blog.Comment where

import           Blog.Types
import           Control.Monad
import           Data.Either
import           Data.Frontmatter
import           Data.List
import           Data.Ord
import           Data.Text                    (Text)
import           Data.Text.Encoding
import qualified Data.Text.IO                 as TIO
import           Data.Time
import           Data.Time.Utils
import           System.Directory
import           System.FilePath
import           Text.Pandoc.Class
import           Text.Pandoc.Extensions
import           Text.Pandoc.Highlighting
import           Text.Pandoc.Options
import           Text.Pandoc.Readers.Markdown
import           Text.Pandoc.Writers.HTML

parseComment ∷ UTCTime → Text → ParseCommentResult
parseComment date' contents' = case parseYamlFrontmatter (encodeUtf8 contents') of
    Done i' r -> ParseCommentResult date' r (fromRight "" $ runPure (writeHtml5 (def {
            writerHighlightStyle = Just haddock
        }) =<< readMarkdown (def {
            readerExtensions = githubMarkdownExtensions
        }) (decodeUtf8 i')))
    Fail _ xs y -> error $ "Failure of " <> (show xs <> y)
    Partial _ -> error "Partial return indicative of failure"

getCommentsIfExists ∷ FilePath → FilePath → IO [ParseCommentResult]
getCommentsIfExists postsDir postId' = do
    commentFiles <- getDirectoryContents $ postsDir </> postId'
    let commentFileNames = (postsDir </>) . (postId' </>) <$> commentFiles
    validCommentFiles <- filterM doesFileExist commentFileNames
    let dates = stringToTime . dropExtension . takeFileName <$> validCommentFiles
    commentTexts <- sequence $ TIO.readFile <$> validCommentFiles
    let commentData = zipWith parseComment dates commentTexts
    pure $ sortOn (Down . commentDate) commentData

getComments ∷ FilePath → FilePath → IO [ParseCommentResult]
getComments postsDir postId' = do
    dirExists <- doesDirectoryExist $ postsDir </> postId'
    if dirExists
        then getCommentsIfExists postsDir postId'
        else pure mempty

{-
commentForm ∷ Text → Text → WebsiteM Html
commentForm postType postId = pure . (H.form
        ! A.class_ "form"
        ! enctype "application/x-www-form-urlencoded"
        ! action ""
        ! method "post"
        ! target "_result") $ do
            H.input ! A.type_ "hidden" ! name "postId" ! value (fromString (T.unpack postId))
            H.input ! A.type_ "hidden" ! name "postType" ! value (fromString (T.unpack postType))
            mapM_ (\(type__, name_, label_, placeholder_) -> H.div ! A.class_ "group" $ do
                H.label ! for name_ $ label_
                H.input ! A.type_ type__ ! A.class_ "form-control" ! name name_ ! placeholder placeholder_
                ) [
                    ("text", "name", "Name", "John Smith"),
                    ("email", "email", "Email", "john@smith.com"),
                    ("text", "website", "Website", "https://mydomain.com")
                    ]
            H.div ! A.class_ "group" $ do
                H.label ! for "name" $ "Comment"
                H.textarea ! A.class_ "form-control" ! name "comment" ! placeholder "I think..." $ mempty
            H.div ! A.class_ "group" $ do
                button ! A.type_ "submit" ! A.class_ "btn btn-primary" $ "Submit"
                H.iframe ! name "_result" ! height "90" ! width "300" ! A.style "border: 0; vertical-align: middle; margin-left: 10px;" $ mempty

renderComment ∷ ParseCommentResult → Html
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
            when (isJust authorUrl) . (a ! href (fromString . T.unpack $ fromJust authorUrl)) $ " (URL)"
            " said on "
            (a ! href ("#" <> fromString (iso8601Show commentDate))) . fromString $ iso8601Show commentDate
            ":"
        p commentHtml
        br

-}
