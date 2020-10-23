{-# LANGUAGE OverloadedStrings #-}

module Blog.Feed where

import           Blog.Types
import           Build.Utils
import           Cheapskate
import           Control.Applicative
import           Control.Monad
import           Data.Aeson                     (FromJSON, Object, (.:), (.:?))
import qualified Data.Aeson                     as A
import           Data.Bifunctor
import           Data.ByteString                (ByteString)
import qualified Data.ByteString.Char8          as B
import           Data.Foldable
import           Data.Frontmatter
import           Data.Function
import           Data.List
import           Data.Maybe
import           Data.Ord
import           Data.String
import           Data.Text                      (Text)
import qualified Data.Text                      as T
import           Data.Text.Encoding
import qualified Data.Text.IO                   as TIO
import           Data.Time
import           Data.Time.Format.ISO8601
import           GHC.Generics
import           Html.Blog.Index
import           Network.Wai.Application.Static
import           Network.Wai.Handler.Warp
import           System.Directory
import           System.FilePath
import qualified Text.Atom.Feed as Atom
import qualified Text.Atom.Feed.Export as Export
import           Text.Blaze.Html5               as H hiding (main)
import           Text.Blaze.Html5.Attributes    as A
import           Text.Blaze.Internal
import           Text.Blaze.Renderer.Pretty
-- import Text.Blaze.Renderer.Utf8
import           Text.XML
import           Util.Triple
import           WaiAppStatic.Types

makeRSSFeed :: [BlogPost] -> Text
makeRSSFeed posts = maybe
  "" (toStrict . renderText def)
  (elementToDoc
     $ Export.xmlFeed
         $ (feed posts)
             {Atom.feedEntries = fmap toEntry posts,
              Atom.feedLinks = [Atom.nullLink "http://example.com/"]})

toEntry :: BlogPost -> Atom.Entry
toEntry (BlogPost metadata html date url content) =
  (Atom.nullEntry
     url -- The ID field. Must be a link to validate.
     (Atom.TextString (take 20 content)) -- Title
     date)
    { Atom.entryAuthors = [Atom.nullPerson {Atom.personName = "Dan Dart"}]
    , Atom.entryLinks = [Atom.nullLink url]
    , Atom.entryContent = Just (Atom.HTMLContent content)
    }

feed :: [BlogPost] -> Atom.Feed
feed posts =
  Atom.nullFeed
    "http://example.com/atom.xml" -- ID
    (Atom.TextString "Example Website") -- Title
    (case posts -- Updated
           of
       BlogPost latestPostDate _ _:_ -> latestPostDate
       _ -> "")