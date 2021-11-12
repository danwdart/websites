{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE UnicodeSyntax     #-}

module Html.MadHacker.Index where

import           Data.Env
import           Data.Site.MadHacker
import           Html.Common.Bootstrap
import           Html.Common.Error.NotFound
import           Html.Common.Head
import           Html.Common.Page
import           Html.MadHacker.Header
import           Text.Blaze.Html5            as H hiding (main)
import           Text.Blaze.Html5.Attributes as A

extraHead ∷ Html
extraHead = link ! rel "alternate" ! type_ "application/atom+xml" ! A.title "The Mad Hacker: Reviews" ! href "/atom.xml"

page ∷ Html → Html → WebsiteM Html
page reviewLinks reviews = do
    header' <- htmlHeader reviewLinks reviews
    head' <- htmlHead descTitle keywords extraHead
    pure . (docTypeHtml ! lang "en-GB") $ do
        head'
        header'

page404 ∷ WebsiteM Html
page404 = defaultPage404 descTitle keywords extraHead
