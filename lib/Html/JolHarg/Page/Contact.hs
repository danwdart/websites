{-# LANGUAGE OverloadedStrings #-}

module Html.JolHarg.Page.Contact where

import           Control.Monad.Reader
import           Data.Env.Types
import           Html.Common.Contact
import           Html.Common.Page
import           Text.Blaze.Html5     as H hiding (main)

pageContact ∷ (MonadReader Website m) ⇒ m Html
pageContact = do
    contactForm' <- contactForm "website@jolharg.com" emailHelpPlural "Website for me..." "I am interested in a website..."
    makePage "contact" "Contact" contactLayout notDefaultPage $ do
        contactForm'
