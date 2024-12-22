{-# LANGUAGE OverloadedStrings #-}

module Html.JolHarg.Page.Contact where

import Control.Lens
import Control.Monad.Reader
import Data.Env.Types
import Data.Text.Encoding
import Html.Common.Contact
import Html.Common.Page
import Text.Blaze.Html5     as H hiding (main)
import Text.Email.Parser

pageContact ∷ (MonadReader Website m) ⇒ m Html
pageContact = do
    email' <- view email
    plainBreadcrumb "Contact" $ do
        contactForm' <- contactForm (textValue (decodeUtf8 (toByteString email'))) emailHelpPlural "Website for me..." "I am interested in a website..."
        makePage "contact" "Contact" contactLayout notDefaultPage $ do
            contactForm'
