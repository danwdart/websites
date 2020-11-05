{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE UnicodeSyntax     #-}

module Blog.Post where

import           Blog.Comment
import           Blog.Types
import           Cheapskate
import           Data.Frontmatter
import           Data.List
import           Data.String
import           Data.Text                   (Text)
import qualified Data.Text                   as T
import           Data.Text.Encoding
import qualified Data.Text.IO                as TIO
import           System.FilePath
import           Text.Blaze.Html5            as H hiding (main)
import           Text.Blaze.Html5.Attributes as A
-- import Text.Blaze.Renderer.Utf8

parseFile ∷ Text → ParseResult
parseFile contents' = case parseYamlFrontmatter (encodeUtf8 contents') of
    Done i' r -> ParseResult r . toMarkup $ markdown def (decodeUtf8 i')
    Fail _ xs y -> error $ "Failure of " <> (show xs <> y)
    _ -> error $ "What is " <> T.unpack contents'


makeBlogPost ∷ FilePath → IO BlogPost
makeBlogPost filename = do
    fileText <- TIO.readFile filename
    let (ParseResult metadata' html') = parseFile fileText
    let postId' = dropExtension $ takeFileName filename
    comments' <- getComments postId'
    return $ BlogPost (T.pack postId') metadata' html' comments'

renderPost ∷ BlogPost → Html
renderPost (BlogPost postId' metadata' html' comments') = do
    a ! name (fromString (T.unpack postId')) $ mempty
    -- Not working in Safari yet, so filter
    img ! height "0" ! width "0" ! src ("/favicon.ico?" <> fromString (T.unpack postId')) ! customAttribute "loading" "lazy"
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
    html'
    br
    h3 "Comments"
    if Data.List.null comments' then
        p "No comments at the moment. Be the first to comment!"
    else
        mapM_ renderComment comments'
    br
    details $ do
        H.summary . (h4 ! A.class_ "d-inline-block") $ "Post a comment"
        commentForm postId'
    hr
