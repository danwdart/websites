{-# LANGUAGE OverloadedStrings #-}

module Html.MadHacker.Index where

import Control.Lens
import Control.Monad.Reader
import Data.Env.Types              as Env
import Html.Common.Blog.Feed
import Html.Common.Error.NotFound
import Html.Common.Head
import Html.MadHacker.Header
import Text.Blaze.Html5            as H hiding (main)
import Text.Blaze.Html5.Attributes as A

page ∷ MonadReader Website m ⇒ Html → Html → Html → m Html
page reviewLinks reviewTagLinks reviews = do
    header' <- htmlHeader reviewLinks reviewTagLinks reviews
    head' <- htmlHead
    pure . (docTypeHtml ! lang "en-GB") $ do
        head'
        header'

page404 ∷ MonadReader Website m ⇒ m Html
page404 = defaultPage404
