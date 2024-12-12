{-# LANGUAGE OverloadedStrings #-}

module Html.MadHacker.Index where

import Control.Monad.Reader
import Data.Env.Types              as Env
import Data.Text                   (Text)
import Html.Common.Blog.Feed
import Html.Common.Error.NotFound
import Html.Common.Head
import Html.MadHacker.Header
import Text.Blaze.Html5            as H hiding (main)
import Text.Blaze.Html5.Attributes as A

page ∷ MonadReader Website m ⇒ Html → Html → Html → (Text → Text) → m Html
page reviewLinks reviewTagLinks reviews titleModifier = do
    title' <- asks Env.title
    description' <- asks Env.description
    url' <- asks Env.url
    imgUrl' <- asks Env.imgUrl
    header' <- htmlHeader reviewLinks reviewTagLinks reviews
    head' <- htmlHead (titleModifier title') description' url' imgUrl' (extraHead title' "/atom.xml")
    pure . (docTypeHtml ! lang "en-GB") $ do
        head'
        header'

page404 ∷ MonadReader Website m ⇒ m Html
page404 = do
    title' <- asks Env.title
    defaultPage404 (extraHead title' "/atom.xml")
