{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-x-partial #-}

module Html.Common.Blog.Post where

import Control.Exception.BlogPostException
import Control.Exception.MissingPostIdException
import Control.Exception.ParseFileException
import Control.Lens
import Control.Monad.Error.Class
import Control.Monad.Reader
import Data.ByteString.Char8                    (ByteString)
import Data.ByteString.Char8                    qualified as BS
import Data.Either
import Data.Env.Types
import Data.Foldable
import Data.Frontmatter
import Data.List.NonEmpty                       qualified as LNE
import Data.NonEmpty                            qualified as NE
import Data.String
import Data.Text                                (Text)
import Data.Text                                qualified as T
import Data.Text.Encoding                       qualified as TE
import Html.Common.Blog.Comment
import Html.Common.Blog.Types                   as BlogTypes
import System.FilePath
import Text.Blaze.Html5                         as H hiding (main)
import Text.Blaze.Html5.Attributes              as A
import Text.Blaze.Internal
import Text.Email.Parser
import Text.Pandoc.Class
import Text.Pandoc.Extensions
import Text.Pandoc.Highlighting
import Text.Pandoc.Options
import Text.Pandoc.Readers.Markdown
import Text.Pandoc.Writers.HTML
-- import Text.Blaze.Html.Renderer.Text (renderHtml)

parseFile ∷ FilePath → ByteString → Either ParseFileException ParseResult
parseFile filename' contents' = case parseYamlFrontmatter contents' of
    Done i' r -> Right $ ParseResult r (fromRight "" $ runPure (writeHtml5 (def {
            writerHighlightMethod = Skylighting haddock
        }) =<< readMarkdown (def {
            readerExtensions = githubMarkdownExtensions
        }) (TE.decodeUtf8Lenient i')))
    Fail inputNotYetConsumed ctxs errMsg -> Left $ ParseFileFailException filename' inputNotYetConsumed ctxs errMsg
    Partial _ -> Left $ ParseFilePartialException filename' contents'

makeBlogPost ∷ (MonadIO m, MonadError BlogPostException m) ⇒ FilePath → FilePath → m BlogPost
makeBlogPost postsDir filename = do
    file <- liftIO $ BS.readFile filename
    case parseFile filename file of
        Left ex -> throwError (BlogPostParseFileException ex) -- wah wah wah
        Right (ParseResult metadata' html') -> do
            let maybePostId' = NE.nonEmpty . T.pack . dropExtension . takeFileName . LNE.head . aliases $ metadata'
            case maybePostId' of
                Just postId' -> do
                    comments' <- modifyError BlogPostCommentException $ getComments postsDir postId'
                    -- TODO stop trusting!
                    pure $ BlogPost postId' metadata' html' comments'
                Nothing -> do
                    throwError $ BlogPostMissingPostIdException MissingPostIdException

tshowChoiceString ∷ ChoiceString → Text
tshowChoiceString (Text text') = text'
tshowChoiceString (Static s') = getText s'
tshowChoiceString (String s') = T.pack s'
tshowChoiceString (ByteString s') = TE.decodeUtf8Lenient s'
tshowChoiceString (PreEscaped s') = tshowChoiceString s'
tshowChoiceString (External s') = tshowChoiceString s'
tshowChoiceString (AppendChoiceString a' b') = tshowChoiceString a' <> tshowChoiceString b'
tshowChoiceString EmptyChoiceString = ""


isLink ∷ StaticString → Bool
isLink ss1 = "a" == getText ss1

isExternalLink ∷ ChoiceString → Bool
isExternalLink acs = "http" `T.isPrefixOf` tshowChoiceString acs

isFeed ∷ ChoiceString → Bool
isFeed acs = ".xml" `T.isSuffixOf` tshowChoiceString acs

{-
-- for debugging only
toText ∷ MarkupM anyMarkup → Text
toText (AddAttribute ss1 ss2 cs m) = T.pack $ printf "AddAttribute %s %s %s: (%s)" (getText ss1) (getText ss2) (tshowChoiceString cs) (toText m)
toText (Parent ss1 ss2 ss3 m) = T.pack $ printf "Parent %s %s %s: (%s)" (getText ss1) (getText ss2) (getText ss3) (toText m)
toText (Append m1 m2) = T.pack $ printf "Append (%s) (%s)" (toText m1) (toText m2)
toText (CustomParent cs m) = T.pack $ printf "CustomParent %s (%s)" (tshowChoiceString cs) (toText m)
toText (Leaf ss1 ss2 ss3 _) = T.pack $ printf "Leaf %s %s %s" (getText ss1) (getText ss2) (getText ss3)
toText (CustomLeaf cs b1 _) = T.pack $ printf "CustomLeaf %s %s" (tshowChoiceString cs) (T.show b1)
toText (Content cs _) = T.pack $ printf "Content %s" (tshowChoiceString cs)
toText (Comment cs _)  = T.pack $ printf "Comment %s" (tshowChoiceString cs)
toText (AddCustomAttribute cs1 cs2 m) = T.pack $ printf "AddCustomAttribute %s %s (%s)" (tshowChoiceString cs1) (tshowChoiceString cs2) (toText m)
toText (Text.Blaze.Internal.Empty _) = "Empty"
-}

setExternalLink ∷ MarkupM anyLink → MarkupM anyLink
setExternalLink (AddAttribute ss1 ss2 cs m) =
    AddAttribute "target" " target=\"" "_blank" .
        AddAttribute "rel" " rel=\"" "noreferrer" $
            AddAttribute ss1 ss2 cs (fixExternalLinks m)
setExternalLink other = fixExternalLinks other

setDownload ∷ MarkupM anyLink → MarkupM anyLink
setDownload (AddAttribute ass1 ass2 acs res) =
    AddAttribute "download" "download=\"" "" $
        AddAttribute ass1 ass2 acs res
setDownload as = as

isLinkHtml ∷ MarkupM anyLink → Bool
isLinkHtml (Parent ss1 _ _ _)          = isLink ss1
isLinkHtml (AddAttribute _ _ _ parent) = isLinkHtml parent
isLinkHtml _                           = False

fixExternalLinks ∷ MarkupM anyLink → MarkupM anyLink
fixExternalLinks at'@(AddAttribute ss1 _ss2 cs parent) =
    if (getText ss1 == "href") && isLinkHtml parent
    then
        if isExternalLink cs then
            setExternalLink at'
        else
            if isFeed cs
            then
                setDownload at'
            else
                AddAttribute ss1 _ss2 cs (fixExternalLinks parent)
    else
        AddAttribute ss1 _ss2 cs (fixExternalLinks parent)
fixExternalLinks (Parent ss1 ss2 ss3 res) = Parent ss1 ss2 ss3 (fixExternalLinks res)
fixExternalLinks (Append m1 m2) = Append (fixExternalLinks m1) (fixExternalLinks m2)
fixExternalLinks (CustomParent cs m1) = CustomParent cs (fixExternalLinks m1)
fixExternalLinks (Leaf ss1 ss2 ss3 a') = Leaf ss1 ss2 ss3 a'
fixExternalLinks (CustomLeaf cs b1 a') = CustomLeaf cs b1 a'
fixExternalLinks (Content cs a') = Content cs a'
fixExternalLinks (Comment cs a') = Comment cs a'
fixExternalLinks (AddCustomAttribute cs1 cs2 m) = AddCustomAttribute cs1 cs2 (fixExternalLinks m)
fixExternalLinks (Text.Blaze.Internal.Empty he) = Text.Blaze.Internal.Empty he

renderPost ∷ (MonadReader Website m) ⇒ BlogPost → m Html
renderPost (BlogPost postId' metadata' html' comments') = do
    email' <- view email
    commentForm' <- commentForm email' (BlogTypes.title metadata')
    renderSuffix' <- preview $ siteType . renderSuffix
    let renderSuffix'' = fold renderSuffix'
    pure . H.article $ do
        a ! name (fromString (T.unpack (NE.getNonEmpty postId'))) $ mempty
        -- Not working in Safari yet, so filter
        h1 . fromString . T.unpack . NE.getNonEmpty $ BlogTypes.title metadata'
        small $ do
            a ! href (fromString . ("/post" <>) . LNE.head . BlogTypes.aliases $ metadata') $ "Permalink"
            " | Author: "
            a ! href (fromString . T.unpack $ "mailto:" <> TE.decodeUtf8Lenient (toByteString email') <> "?subject=" <> NE.getNonEmpty (BlogTypes.title metadata')) $ "Dan Dart"
            " | Published: "
            fromString . show . date $ metadata'
            " | Tags: "
            foldMap' ((\str -> do
                a ! rel "tag" ! href (textValue ("/tag/" <> str)) $ text str
                " "
                ) . NE.getNonEmpty . getTag) (LNE.sort $ tags metadata')
        br
        br
        foldMap' (\x -> (H.div ! class_ "row") . (H.div ! class_ "col text-center") $ img ! class_ "img-fluid" ! alt "Featured image for article" ! A.title "Featured image for article" ! src (toValue (show x))) $ featuredImage metadata'
        br
        fixExternalLinks html'
        br
        renderSuffix'' metadata'
        br
        section $ do
            h3 "Comments"
            if Prelude.null comments' then small "No comments yet..." <> br else fixExternalLinks $ traverse_ renderComment comments'
            br
            h4 "Post a comment:"
            commentForm'
            br
