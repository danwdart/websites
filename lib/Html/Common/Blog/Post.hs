{-# LANGUAGE OverloadedStrings #-}

module Html.Common.Blog.Post where

import           Control.Exception
import           Control.Exception.ParseFileException
import           Data.Either
import           Data.Env.Types
import           Data.Frontmatter
import           Data.String
import           Data.Text                    (Text)
import qualified Data.Text                    as T
import           Data.Text.Encoding
import           Html.Common.Blog.Comment
import           Html.Common.Blog.Types       as BlogTypes
import           System.FilePath
import           Text.Blaze.Html5             as H hiding (main)
import           Text.Blaze.Html5.Attributes  as A
import           Text.Blaze.Internal
import           Text.Pandoc.Class
import           Text.Pandoc.Extensions
import           Text.Pandoc.Highlighting
import           Text.Pandoc.Options
import           Text.Pandoc.Readers.Markdown
import           Text.Pandoc.Writers.HTML

parseFile ∷ Text → Either ParseFileException ParseResult
parseFile contents' = case parseYamlFrontmatter (encodeUtf8 contents') of
    Done i' r -> Right $ ParseResult r (fromRight "" $ runPure (writeHtml5 (def {
            writerHighlightStyle = Just haddock
        }) =<< readMarkdown (def {
            readerExtensions = githubMarkdownExtensions
        }) (decodeUtf8 i')))
    Fail i' xs' y' -> Left $ PFFail i' xs' y'
    Partial _ -> Left PFPartial

makeBlogPost ∷ FilePath → FilePath → IO BlogPost
makeBlogPost postsDir filename = do
    fileString <- readFile filename
    let fileText = T.pack fileString
    case parseFile fileText of
        Left ex -> throwIO ex -- wah wah wah
        Right (ParseResult metadata' html') -> do
            let postId' = dropExtension . takeFileName . Prelude.head . aliases $ metadata'
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
renderPost email' renderSuffix (BlogPost postId' metadata' html' comments') = do
    commentForm' <- commentForm email' (BlogTypes.title metadata')
    pure $ do
        a ! name (fromString (T.unpack postId')) $ mempty
        -- Not working in Safari yet, so filter
        h1 . fromString . T.unpack $ BlogTypes.title metadata'
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
        maybe mempty (\x -> (H.div ! class_ "row") . (H.div ! class_ "col text-center") $ img ! class_ "img-fluid" ! src (textValue x)) $ featuredImage metadata'
        br
        fixExternalLinks html'
        br
        renderSuffix metadata'
        br
        h3 "Comments"
        if Prelude.null comments' then small "No comments yet..." <> br else mapM_ renderComment comments'
        br
        h4 "Post a comment:"
        commentForm'
        br
