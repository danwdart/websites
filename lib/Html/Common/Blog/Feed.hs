{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-x-partial #-}

module Html.Common.Blog.Feed where

import Control.Lens
-- import Control.Monad (when)
import Control.Monad.Reader
import Data.ByteString.Char8         as BS
import Data.Env.Types                as Env
import Data.List.NonEmpty            (NonEmpty)
import Data.List.NonEmpty            qualified as LNE
-- import Data.Maybe
import Data.NonEmpty                 qualified as NE
import Data.Text                     (Text)
import Data.Text                     qualified as T
import Data.Text.Encoding            qualified as TE
import Data.Text.NonEmpty            (NonEmptyText)
import Data.Text.Lazy                qualified as TL
import Html.Common.Blog.Types
import Network.URI                   qualified as NetURI
import Text.Atom.Feed                qualified as Atom
import Text.Atom.Feed.Export         qualified as Export
import Text.Blaze.Html.Renderer.Text (renderHtml)
import Text.Blaze.Html5              as H hiding (main)
import Text.Blaze.Html5.Attributes   as A

extraHead ∷ MonadReader Website m ⇒ m Html
extraHead = do -- TODO maybeT
    mAtomTitle' <- preview $ siteType . atomTitle
    mAtomUrl' <- preview $ siteType . atomUrl
    -- let's do this instead of being terse af
    case mAtomTitle' of
        Nothing -> pure mempty
        Just atomTitle' -> case mAtomUrl' of
            Nothing -> pure mempty
            Just atomUrl' -> pure $
                -- toValue or others?
                link ! rel "alternate" ! type_ "application/atom+xml" ! A.title (textValue (NE.getNonEmpty atomTitle')) ! href (toValue (show atomUrl'))

toEntry ∷ ByteString → BlogPost → Atom.Entry
toEntry domain (BlogPost _ BlogMetadata { aliases = aliases', title = title', date = date' } html' _) = (
        Atom.nullEntry
        (Prelude.last . T.splitOn "/" . T.pack $ LNE.head aliases') -- The ID field. Must be a link to validate.
        (Atom.TextString (NE.getNonEmpty title'))
        (T.show date')
    )
    { Atom.entryAuthors = [
        Atom.nullPerson {
            Atom.personName = "Dan Dart"
        }
        ]
    , Atom.entryLinks = [
        Atom.nullLink (TE.decodeUtf8Lenient domain <> "/post" <> T.pack (LNE.head aliases'))
        ]
    , Atom.entryContent = Just (Atom.HTMLContent . TL.toStrict . renderHtml $ html')
    }

dateUpdated ∷ NonEmpty BlogPost → Text
dateUpdated = T.show . date . metadata . LNE.head

feed ∷ NetURI.URI → NonEmptyText → NonEmpty BlogPost → Atom.Feed
feed atomXml title' posts = Atom.nullFeed
    (T.pack $ show atomXml) -- ID
    (Atom.TextString (NE.getNonEmpty title')) -- Title
    (dateUpdated posts)

makeRSSFeed ∷ NetURI.URI → NetURI.URI → NetURI.URI → NonEmptyText → NonEmpty BlogPost → Maybe NonEmptyText
makeRSSFeed atomXml selfUrl domain title' posts = NE.nonEmpty . TL.toStrict =<< (
    Export.textFeed $
    (feed atomXml title' posts)
    {
        Atom.feedEntries = toEntry (BS.pack $ show domain) <$> LNE.toList posts,
        Atom.feedLinks = [
            Atom.nullLink (T.show selfUrl)
            ]
        }
    )
