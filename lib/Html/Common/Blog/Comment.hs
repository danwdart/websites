
{-# LANGUAGE OverloadedStrings #-}

module Html.Common.Blog.Comment where

import           Control.Monad
import           Data.Either
import           Data.Env.Types
import           Data.Frontmatter
import qualified Data.List                    as L
import           Data.Maybe
import           Data.Ord
import           Data.String
import           Data.Text                    (Text)
import qualified Data.Text                    as T
import           Data.Text.Encoding
import qualified Data.Text.IO                 as TIO
import           Data.Time
import           Data.Time.Format.ISO8601
import           Data.Time.Utils
import           Html.Common.Blog.Types
import           Html.Common.Icon             as Icon
import           System.Directory
import           System.FilePath
import           Text.Blaze.Html5             as H hiding (main)
import           Text.Blaze.Html5.Attributes  as A
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
    commentTexts <- mapM TIO.readFile validCommentFiles
    let commentData = zipWith parseComment dates commentTexts
    pure $ L.sortOn (Down . commentDate) commentData

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
