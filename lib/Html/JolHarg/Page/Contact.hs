{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE UnicodeSyntax     #-}

module Html.JolHarg.Page.Contact where

import           Control.Monad.Reader
import           Data.Env
import           Data.Site.JolHarg
import           Html.Common.Bootstrap
import           Html.Common.Card
import           Html.Common.Contact
import           Html.Common.Error.NotFound
import           Html.Common.Head
import           Html.Common.Header
import           Html.Common.Link
import           Html.Common.Page
import           Text.Blaze.Html5            as H hiding (main)
import           Text.Blaze.Html5.Attributes as A

pageContact ∷ WebsiteM Html
pageContact = do
    contactForm' <- contactForm "website@jolharg.com" emailHelpPlural "Website for me..." "I am interested in a website..."
    makePage "contact" "Contact" contactLayout notDefaultPage $ do
        contactForm'
