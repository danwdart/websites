{-# LANGUAGE OverloadedStrings #-}

module Blog.Feed where

import Blog.Types
    ( BlogPost(BlogPost, metadata), BlogMetadata(BlogMetadata, date) )
import qualified Text.Atom.Feed as Atom
import qualified Text.Atom.Feed.Export as Export
import Data.Text ( Text )
import Data.Text.Lazy (toStrict)
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import Text.Blaze.Html.Renderer.Text ( renderHtml )

toEntry :: BlogPost -> Atom.Entry
toEntry (BlogPost postId (BlogMetadata title date' _ _ _) html _) = (
        Atom.nullEntry
        ("https://blog.dandart.co.uk/#" <> postId) -- The ID field. Must be a link to validate.
        (Atom.TextString title)
        (T.pack . show $ date')
    )
    { Atom.entryAuthors = [Atom.nullPerson {Atom.personName = "Dan Dart"}]
    , Atom.entryLinks = [Atom.nullLink ("https://blog.dandart.co.uk/#" <> postId)]
    , Atom.entryContent = Just (Atom.HTMLContent . toStrict . renderHtml $ html)
    }

feed :: [BlogPost] -> Atom.Feed
feed posts = Atom.nullFeed
    "https://dandart.co.uk/atom.xml" -- ID
    (Atom.TextString "Dan Dart's Blog") -- Title
    (T.pack . show . date . metadata . Prelude.head $ posts)

makeRSSFeed :: [BlogPost] -> Text
makeRSSFeed posts = maybe "" TL.toStrict (
    Export.textFeed $
    (feed posts)
    {
        Atom.feedEntries = fmap toEntry posts,
        Atom.feedLinks = [
            Atom.nullLink "https://dandart.co.uk/"
            ]
        }
    )