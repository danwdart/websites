{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE UnicodeSyntax     #-}

module Html.JolHarg.Page.Contact where

import           Data.Env.Types
import           Html.Common.Contact
import           Html.Common.Page
import           Text.Blaze.Html5            as H hiding (main)

pageContact ∷ WebsiteM Html
pageContact = do
    contactForm' <- contactForm "website@jolharg.com" emailHelpPlural "Website for me..." "I am interested in a website..."
    makePage "contact" "Contact" contactLayout notDefaultPage $ do
        contactForm'
