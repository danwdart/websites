{-# LANGUAGE OverloadedStrings #-}

module Site.Blog where

import           Blog.Comment
import           Blog.Feed
import           Blog.Link
import           Blog.Post
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
import qualified Text.Atom.Feed                 as Atom
import qualified Text.Atom.Feed.Export          as Export
import           Text.Blaze.Html5               as H hiding (main)
import           Text.Blaze.Html5.Attributes    as A
import           Text.Blaze.Internal
import           Text.Blaze.Renderer.Pretty
import           Text.XML
import           Util.Triple
import           WaiAppStatic.Types

build :: IO ()
build = do
    files <- getDirectoryContents "posts"
    let fileNames = ("posts/" </>) <$> files -- if used in same line, use Compose
    validFiles <- filterM doesFileExist fileNames
    posts <- sequence $ makeBlogPost <$> validFiles
    let sortedPosts = sortOn (Down . date . metadata) . filter (not . draft . metadata) $ posts
    let renderedPosts = foldMap renderPost sortedPosts
    TIO.writeFile ".sites/blog/feed.rss" $ makeRSSFeed sortedPosts
    let renderedLinks = makeLinks sortedPosts
    make "blog" $ page renderedLinks renderedPosts

serve :: IO ()
serve = do
    putStrLn "Building..."
    build
    putStrLn "Serving..."
    runEnv 80 . staticApp $ (defaultWebAppSettings ".sites/blog/"){ ssIndices = mapMaybe toPiece ["index.html"] }
