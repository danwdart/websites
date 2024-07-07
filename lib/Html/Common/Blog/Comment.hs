
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Unsafe            #-}
{-# OPTIONS_GHC -Wno-unsafe #-}

module Html.Common.Blog.Comment where

import Control.Exception
import Control.Exception.ParseFileException
import Control.Monad
import Data.ByteString.Char8                qualified as B
import Data.Either
import Data.Env.Types
import Data.Frontmatter
import Data.List                            qualified as L
import Data.Ord
import Data.String
import Data.Text                            (Text)
import Data.Text                            qualified as T
import Data.Text.Encoding
import Data.Time
import Data.Time.Format.ISO8601
import Data.Time.Utils
import Html.Common.Blog.Types
import Html.Common.Icon                     as Icon
import System.Directory
import System.FilePath
import Text.Blaze.Html5                     as H hiding (main)
import Text.Blaze.Html5.Attributes          as A
import Text.Pandoc.Class
import Text.Pandoc.Extensions
import Text.Pandoc.Highlighting
import Text.Pandoc.Options
import Text.Pandoc.Readers.Markdown
import Text.Pandoc.Writers.HTML

parseComment ∷ UTCTime → Text → Either ParseFileException ParseCommentResult
parseComment date' contents' = case parseYamlFrontmatter (encodeUtf8 contents') of
    Done i' r -> Right $ ParseCommentResult date' r (fromRight "" $ runPure (writeHtml5 (def {
            writerHighlightStyle = Just haddock
        }) =<< readMarkdown (def {
            readerExtensions = githubMarkdownExtensions
        }) (decodeUtf8 i')))
    Fail i' xs' y' -> Left $ PFFail i' xs' y'
    Partial _ -> Left PFPartial

getCommentsIfExists ∷ FilePath → FilePath → IO [ParseCommentResult]
getCommentsIfExists postsDir postId' = do
    commentFiles <- getDirectoryContents $ postsDir </> postId'
    let commentFileNames = (postsDir </>) . (postId' </>) <$> commentFiles
    validCommentFiles <- filterM doesFileExist commentFileNames
    let mDates = traverse (stringToTime . dropExtension . takeFileName) validCommentFiles
    case mDates of
        Left ex -> throwIO ex
        Right dates -> do
            commentTexts <- fmap decodeUtf8 <$> mapM B.readFile validCommentFiles
            case zipWithM parseComment dates commentTexts of
                Left ex -> throwIO ex
                Right commentData -> pure $ L.sortOn (Down . commentDate) commentData

getComments ∷ FilePath → FilePath → IO [ParseCommentResult]
getComments postsDir postId' = do
    dirExists <- doesDirectoryExist $ postsDir </> postId'
    if dirExists
        then getCommentsIfExists postsDir postId'
        else pure mempty

commentForm ∷ Text → Text → WebsiteM Html
commentForm toEmail title' = pure . (H.form
        ! A.class_ "form"
        ! enctype "application/x-www-form-urlencoded"
        ! action "mailto:"
        ! method "get"
        ! target "email") $ do
            H.input ! A.type_ "hidden" ! name "to" ! value (textValue toEmail)
            H.input ! A.type_ "hidden" ! name "subject" ! value (textValue $ "Re: " <> title')
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
            authorUrl' = maybe mempty (fromString . T.unpack) authorUrl
        small $ do
            a ! name (fromString (iso8601Show commentDate)) $ mempty
            a ! href ("mailto:" <> (fromString . T.unpack $ authorEmail)) $ string (T.unpack author)
            " "
            a ! href authorUrl' $ " (URL)"
            " said on "
            (a ! href ("#" <> fromString (iso8601Show commentDate))) . fromString $ iso8601Show commentDate
            ":"
        p commentHtml
        br
