{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE UnicodeSyntax     #-}

module Blog.Post where

import           Blog.Comment
import           Blog.Types
import           Data.Either
import           Data.Env
import           Data.Frontmatter
import           Data.List
import           Data.String
import           Data.Text                    (Text)
import qualified Data.Text                    as T
import           Data.Text.Encoding
import qualified Data.Text.IO                 as TIO
import           Html.Common.Visit
import           System.FilePath
import           Text.Blaze.Html5             as H hiding (main)
import           Text.Blaze.Html5.Attributes  as A
import           Text.Blaze.Internal
import           Text.Pandoc.Class
-- import           Text.Pandoc.Definitions
import           Text.Pandoc.Extensions
import           Text.Pandoc.Highlighting
import           Text.Pandoc.Options
import           Text.Pandoc.Readers.Markdown
import           Text.Pandoc.Writers.HTML

parseFile ∷ Text → ParseResult
parseFile contents' = case parseYamlFrontmatter (encodeUtf8 contents') of
    Done i' r -> ParseResult r (fromRight "" $ runPure (writeHtml5 (def {
            writerHighlightStyle = Just haddock
        }) =<< readMarkdown (def {
            readerExtensions = githubMarkdownExtensions
        }) (decodeUtf8 i')))
    Fail _ xs y -> error $ "Failure of " <> (show xs <> y)
    _ -> error $ "What is " <> T.unpack contents'

makeBlogPost ∷ FilePath → FilePath → IO BlogPost
makeBlogPost postsDir filename = do
    fileText <- TIO.readFile filename
    let (ParseResult metadata' html') = parseFile fileText
    let postId' = dropExtension $ takeFileName filename
    comments' <- getComments postsDir postId'
    pure $ BlogPost (T.pack postId') metadata' html' comments'

tshowChoiceString ∷ ChoiceString → Text
tshowChoiceString (Text s) = s
tshowChoiceString _        = ""

isLink ∷ StaticString → Bool
isLink ss1 = "a" == getText ss1

isExternalLink ∷ ChoiceString → Bool
isExternalLink acs = "http" `T.isPrefixOf` tshowChoiceString acs

isFeed ∷ ChoiceString → Bool
isFeed acs = ".xml" `T.isSuffixOf` tshowChoiceString acs

setExternalLink ∷ MarkupM a → MarkupM a
setExternalLink (AddAttribute ass1 ass2 acs res) =
    AddAttribute "target" "target=\"" "_blank" .
        AddAttribute "rel" "rel=\"" "noreferrer" $
            AddAttribute ass1 ass2 acs res
setExternalLink as = as

setDownload ∷ MarkupM a → MarkupM a
setDownload (AddAttribute ass1 ass2 acs res) =
    AddAttribute "download" "download=\"" "" $
        AddAttribute ass1 ass2 acs res
setDownload as = as

fixExternalLinks ∷ MarkupM a → MarkupM a
fixExternalLinks at@(AddAttribute _ _ acs (Parent ss1 _ _ _)) =
    if isLink ss1 then
        if isExternalLink acs
        then
            setExternalLink at
        else
            if isFeed acs
            then
                setDownload at
            else
                at
    else
        at
fixExternalLinks (Parent ss1 ss2 ss3 res) = Parent ss1 ss2 ss3 (fixExternalLinks res)
fixExternalLinks (Append m1 m2) = Append (fixExternalLinks m1) (fixExternalLinks m2)
fixExternalLinks as = as

renderPost ∷ Text → (BlogMetadata → Html) → BlogPost → WebsiteM Html
renderPost postType renderSuffix (BlogPost postId' metadata' html' comments') = do
    commentForm' <- commentForm postType postId'
    let visitPageSub' = visitPageSub (fromString . T.unpack $ postType) (fromString . T.unpack $ postId')
    visitPageSubTop <- visitPageSub' "top"
    visitPageSubBottom <- visitPageSub' "bottom"
    pure $ do
        a ! name (fromString (T.unpack postId')) $ mempty
        -- Not working in Safari yet, so filter
        visitPageSubTop
        h1 . fromString . T.unpack $ Blog.Types.title metadata'
        small $ do
            a ! href ("#" <> fromString (T.unpack postId')) $ "Permalink"
            " | Published: "
            fromString . show . date $ metadata'
            " | Tags: "
            foldMap ((\str -> do
                a ! href "#" {-( <> fromString str)-} $ fromString str
                " "
                ) . T.unpack . getTag) (tags metadata')
        br
        br
        fixExternalLinks html'
        br
        renderSuffix metadata'
        br
        h3 "Comments"
        if Data.List.null comments' then
            p "No comments at the moment. Be the first to comment!"
        else
            mapM_ renderComment comments'
        br
        details $ do
            H.summary . (h4 ! A.class_ "d-inline-block") $ "Post a comment"
            commentForm'
        hr
        visitPageSubBottom
