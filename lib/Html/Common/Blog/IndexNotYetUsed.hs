{-# LANGUAGE OverloadedStrings #-}

module Html.Common.Blog.Index where

import Control.Monad.Reader
import Data.ByteString.Char8       (ByteString)
import Data.Env.Types              as Env
import Data.Text                   (Text)
import Data.Text.Encoding
import Html.Common.Error.NotFound
import Html.Common.Head
import Text.Blaze.Html5            as H hiding (main)
import Text.Blaze.Html5.Attributes as A

page ∷ MonadReader Website m ⇒ Html → Html → Html → m Html
page blogPostLinks blogTagLinks blogPosts = do
    header' <- htmlHeader blogPostLinks blogTagLinks blogPosts
    head' <- htmlHead
    pure . (docTypeHtml ! lang "en-GB") $ do
        head'
        header'

page404 ∷ MonadReader Website m ⇒ m Html
page404 = defaultPage404
