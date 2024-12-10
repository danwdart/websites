{-# LANGUAGE OverloadedStrings #-}

module Html.MadHacker.Index where

import Control.Monad.Reader
import Data.Env.Types
import Data.Site.MadHacker
import Html.Common.Error.NotFound
import Html.Common.Head
import Html.MadHacker.Header
import Text.Blaze.Html5            as H hiding (main)
import Text.Blaze.Html5.Attributes as A

extraHead ∷ Html
extraHead = link ! rel "alternate" ! type_ "application/atom+xml" ! A.title "The Mad Hacker: Reviews" ! href "/atom.xml"

page ∷ MonadReader Website m ⇒ Html → Html → Html → m Html
page reviewLinks reviewTagLinks reviews = do
    header' <- htmlHeader reviewLinks reviewTagLinks reviews
    head' <- htmlHead title' description' url' imgUrl extraHead
    pure . (docTypeHtml ! lang "en-GB") $ do
        head'
        header'

page404 ∷ MonadReader Website m ⇒ m Html
page404 = defaultPage404 title' description' url' imgUrl extraHead
