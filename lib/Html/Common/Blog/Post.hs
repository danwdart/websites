{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-x-partial #-}

module Html.Common.Blog.Post where

import Control.Exception.BlogPostException
import Control.Exception.MissingPostIdException
import Control.Exception.ParseFileException
import Control.Lens
import Control.Monad.Error.Class
import Control.Monad.Reader
import Data.ByteString.Char8                (ByteString)
import Data.ByteString.Char8                qualified as BS
import Data.Either
import Data.Env.Types
import Data.Foldable
import Data.Frontmatter
import Data.List.NonEmpty                   qualified as LNE
import Data.NonEmpty                        qualified as NE
import Data.String
import Data.Text                            (Text)
import Data.Text                            qualified as T
import Data.Text.Encoding                   qualified as TE
import Html.Common.Blog.Comment
import Html.Common.Blog.Types               as BlogTypes
import System.FilePath
import Text.Blaze.Html5                     as H hiding (main)
import Text.Blaze.Html5.Attributes          as A
import Text.Blaze.Internal
import Text.Email.Parser
import Text.Pandoc.Class
import Text.Pandoc.Extensions
import Text.Pandoc.Highlighting
import Text.Pandoc.Options
import Text.Pandoc.Readers.Markdown
import Text.Pandoc.Writers.HTML

parseFile ∷ FilePath → ByteString → Either ParseFileException ParseResult
parseFile filename' contents' = case parseYamlFrontmatter contents' of
    Done i' r -> Right $ ParseResult r (fromRight "" $ runPure (writeHtml5 (def {
            writerHighlightStyle = Just haddock
        }) =<< readMarkdown (def {
            readerExtensions = githubMarkdownExtensions
        }) (TE.decodeUtf8Lenient i')))
    Fail inputNotYetConsumed ctxs errMsg -> Left $ PFFail filename' inputNotYetConsumed ctxs errMsg
    Partial _ -> Left $ PFPartial filename' contents'

makeBlogPost ∷ (MonadIO m, MonadError BlogPostException m) => FilePath → FilePath → m BlogPost
makeBlogPost postsDir filename = do
    file <- liftIO $ BS.readFile filename
    case parseFile filename file of
        Left ex -> throwError (BPPFEx ex) -- wah wah wah
        Right (ParseResult metadata' html') -> do
            let maybePostId' = NE.nonEmpty . T.pack . dropExtension . takeFileName . LNE.head . aliases $ metadata'
            case maybePostId' of
                Just postId' -> do
                    comments' <- modifyError BPCEx $ getComments postsDir postId'
                    -- TODO stop trusting!
                    pure $ BlogPost postId' metadata' html' comments'
                Nothing -> do
                    throwError $ BPMPIEx MissingPostIdException

tshowChoiceString ∷ ChoiceString → Text
tshowChoiceString (Text text') = text'
tshowChoiceString _            = ""

isLink ∷ StaticString → Bool
isLink ss1 = "a" == getText ss1

isExternalLink ∷ ChoiceString → Bool
isExternalLink acs = "http" `T.isPrefixOf` tshowChoiceString acs

isFeed ∷ ChoiceString → Bool
isFeed acs = ".xml" `T.isSuffixOf` tshowChoiceString acs

setExternalLink ∷ MarkupM anyLink → MarkupM anyLink
setExternalLink (AddAttribute ass1 ass2 acs res) =
    AddAttribute "target" "target=\"" "_blank" .
        AddAttribute "rel" "rel=\"" "noreferrer" $
            AddAttribute ass1 ass2 acs res
setExternalLink as = as

setDownload ∷ MarkupM anyLink → MarkupM anyLink
setDownload (AddAttribute ass1 ass2 acs res) =
    AddAttribute "download" "download=\"" "" $
        AddAttribute ass1 ass2 acs res
setDownload as = as

fixExternalLinks ∷ MarkupM anyLink → MarkupM anyLink
fixExternalLinks at'@(AddAttribute _ _ acs (Parent ss1 _ _ _)) =
    if isLink ss1 then
        if isExternalLink acs
        then
            setExternalLink at'
        else
            if isFeed acs
            then
                setDownload at'
            else
                at'
    else
        at'
fixExternalLinks (Parent ss1 ss2 ss3 res) = Parent ss1 ss2 ss3 (fixExternalLinks res)
fixExternalLinks (Append m1 m2) = Append (fixExternalLinks m1) (fixExternalLinks m2)
fixExternalLinks as = as

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
        foldMap' (\x -> (H.div ! class_ "row") . (H.div ! class_ "col text-center") $ img ! class_ "img-fluid" ! src (toValue (show x))) $ featuredImage metadata'
        br
        fixExternalLinks html'
        br
        renderSuffix'' metadata'
        br
        section $ do
            h3 "Comments"
            if Prelude.null comments' then small "No comments yet..." <> br else traverse_ renderComment comments'
            br
            h4 "Post a comment:"
            commentForm'
            br
