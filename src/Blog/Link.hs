{-# LANGUAGE OverloadedStrings #-}
module Blog.Link where

import           Blog.Types
import           Blog.Post (getPostId)
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
import           Util.List
import           Util.Time
import           Util.Triple
import           WaiAppStatic.Types

renderMetaLink :: BlogMetadata -> Html
renderMetaLink m = a ! href (fromString ("#" <> getPostId m)) $ fromString (T.unpack (Blog.Types.title m))

renderLink :: BlogPost -> Html
renderLink = renderMetaLink . metadata

makeLinks :: [BlogPost] -> Html
makeLinks = foldMap ((
        \byYear -> do
            details ! customAttribute "open" "" ! class_ "pl-2" $ do
                H.summary $ fromString . show . year . date . metadata . Data.List.head . Data.List.head $ byYear
                p $ foldMap (
                    \byMonth -> details! customAttribute "open" "" ! class_ "pl-2" $ do
                        -- "%B" is Month
                        H.summary $ fromString . formatTime defaultTimeLocale "%B" . date . metadata . Data.List.head $ byMonth
                        p $ foldMap (\link -> do
                            p ! class_ "pl-2" $ renderLink link
                            br
                            ) byMonth
                    ) byYear
        ) .
        groupOn (month . date . metadata)
    ) . groupOn (year . date . metadata)