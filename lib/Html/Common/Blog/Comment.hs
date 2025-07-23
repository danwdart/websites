
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Unsafe            #-}
{-# OPTIONS_GHC -Wno-unsafe #-}

module Html.Common.Blog.Comment where

import Control.Exception.CommentException
import Control.Exception.ParseFileException
import Control.Monad
import Control.Monad.Error.Class
import Control.Monad.Reader
import Data.ByteString.Char8                (ByteString)
import Data.ByteString.Char8                qualified as BS
import Data.Either
import Data.Env.Types
import Data.Frontmatter
import Data.List                            qualified as L
import Data.NonEmpty                        qualified as NE
import Data.Ord
import Data.String
import Data.Text                            qualified as T
import Data.Text.NonEmpty                   (NonEmptyText)
import Data.Text.Encoding                   qualified as TE
import Data.Time
import Data.Time.Format.ISO8601
import Data.Time.Utils
import Html.Common.Blog.Types
import Html.Common.Icon                     as Icon
import System.Directory
import System.FilePath
import Text.Blaze.Html5                     as H hiding (main)
import Text.Blaze.Html5.Attributes          as A
import Text.Email.Parser
import Text.Pandoc.Class
import Text.Pandoc.Extensions
import Text.Pandoc.Highlighting
import Text.Pandoc.Options
import Text.Pandoc.Readers.Markdown
import Text.Pandoc.Writers.HTML

parseComment ∷ FilePath → UTCTime → ByteString → Either ParseFileException ParseCommentResult
parseComment filename' date' contents' = case parseYamlFrontmatter contents' of
    Done i' r -> Right $ ParseCommentResult date' r (fromRight "" $ runPure (writeHtml5 (def {
            writerHighlightStyle = Just haddock
        }) =<< readMarkdown (def {
            readerExtensions = githubMarkdownExtensions
        }) (TE.decodeUtf8Lenient i')))
    Fail inputNotYetConsumed ctxs errMsg -> Left $ ParseFileFailException filename' inputNotYetConsumed ctxs errMsg
    Partial _ -> Left $ ParseFilePartialException filename' contents'

getCommentsIfExists ∷ (MonadIO m, MonadError CommentException m) => FilePath → NonEmptyText → m [ParseCommentResult]
getCommentsIfExists postsDir postId' = do
    commentFiles <- liftIO . getDirectoryContents $ postsDir </> T.unpack (NE.getNonEmpty postId')
    let commentFileNames = (postsDir </>) . (T.unpack (NE.getNonEmpty postId') </>) <$> commentFiles
    validCommentFiles <- filterM (liftIO . doesFileExist) commentFileNames
    let mDates = traverse (stringToTime . dropExtension . takeFileName) validCommentFiles
    case mDates of
        Left ex -> throwError (CommentInvalidDateException ex)
        Right dates -> do
            commentTexts <- traverse (liftIO . BS.readFile) validCommentFiles
            -- Get replies
            case sequenceA (zipWith3 parseComment validCommentFiles dates commentTexts) of
                Left ex -> throwError (CommentParseFileException ex)
                Right commentData -> pure $ L.sortOn (Down . commentDate) commentData

getComments ∷ (MonadIO m, MonadError CommentException m) => FilePath → NonEmptyText → m [ParseCommentResult]
getComments postsDir postId' = do
    dirExists <- liftIO . doesDirectoryExist $ postsDir </> T.unpack (NE.getNonEmpty postId')
    if dirExists
        then getCommentsIfExists postsDir postId'
        else pure mempty

commentForm ∷ MonadReader Website m ⇒ EmailAddress → NonEmptyText → m Html
commentForm toEmail title' = pure . (H.form
        ! A.class_ "form"
        ! enctype "application/x-www-form-urlencoded"
        ! action "mailto:" -- intentionally empty
        ! method "get"
        ! target "email") $ do
            -- less scraping happens? maybe?
            H.input ! A.type_ "hidden" ! name "to" ! value (textValue (TE.decodeUtf8Lenient (toByteString toEmail)))
            H.input ! A.type_ "hidden" ! name "subject" ! value (textValue $ "Re: " <> NE.getNonEmpty title')
            H.div ! A.class_ "group" $ do
                H.label ! for "body" $ do
                    "Comment"
                    H.textarea ! A.class_ "form-control" ! name "body" ! placeholder "I think..." $ mempty
            br
            H.div ! A.class_ "group" $ do
                button ! A.type_ "submit" ! A.class_ "btn btn-primary" $ do
                    Icon.icon S "envelope"
                    " Send"

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
        let authorUrl' ∷ AttributeValue
            authorUrl' = foldMap (toValue . show) authorUrl
        small $ do
            a ! name (fromString (iso8601Show commentDate)) $ mempty
            a ! href ("mailto:" <> (textValue . NE.getNonEmpty $ authorEmail)) $ text (NE.getNonEmpty author)
            " "
            a ! href authorUrl' ! A.target "_blank" ! A.rel "noreferrer" $ " (URL)"
            " said on "
            (a ! href ("#" <> fromString (iso8601Show commentDate))) . fromString $ iso8601Show commentDate
            ":"
        p commentHtml
        br
