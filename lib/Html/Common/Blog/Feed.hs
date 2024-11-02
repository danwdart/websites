{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-x-partial #-}

module Html.Common.Blog.Feed where

import Data.List.NonEmpty            (NonEmpty)
import Data.List.NonEmpty            qualified as LNE
import Data.Text                     (Text)
import Data.Text.Lazy                (toStrict)
import Data.Text.Lazy                qualified as TL
import Html.Common.Blog.Types
import Text.Atom.Feed                qualified as Atom
import Text.Atom.Feed.Export         qualified as Export
import Text.Blaze.Html.Renderer.Text (renderHtml)
import Text.Pandoc.Shared            (tshow)

toEntry ∷ Text → BlogPost → Atom.Entry
toEntry domain (BlogPost postId' BlogMetadata { title = title', date = date' } html' _) = (
        Atom.nullEntry
        (domain <> "#" <> postId') -- The ID field. Must be a link to validate.
        (Atom.TextString title')
        (tshow date')
    )
    { Atom.entryAuthors = [Atom.nullPerson {Atom.personName = "Dan Dart"}]
    , Atom.entryLinks = [Atom.nullLink (domain <> "/#" <> postId')]
    , Atom.entryContent = Just (Atom.HTMLContent . toStrict . renderHtml $ html')
    }

dateUpdated ∷ NonEmpty BlogPost → Text
dateUpdated = tshow . date . metadata . LNE.head

feed ∷ Text → Text → NonEmpty BlogPost → Atom.Feed
feed domain title' posts = Atom.nullFeed
    (domain <> "atom.xml") -- ID
    (Atom.TextString title') -- Title
    (dateUpdated posts)

makeRSSFeed ∷ Text → Text → NonEmpty BlogPost → Text
makeRSSFeed domain title' posts = maybe "" TL.toStrict (
    Export.textFeed $
    (feed domain title' posts)
    {
        Atom.feedEntries = toEntry domain <$> LNE.toList posts,
        Atom.feedLinks = [
            Atom.nullLink domain
            ]
        }
    )
