{-# LANGUAGE OverloadedStrings #-}

module Html.DanDart.Page.Contact where

import           Control.Monad.Reader
import           Data.Env.Types
import           Html.Common.Contact
import           Html.Common.Page
import           Text.Blaze.Html5     as H hiding (main)

pageContact ∷ (MonadReader Website m) ⇒ m Html
pageContact = do
    contactForm' <- contactForm "website@dandart.co.uk" emailHelpSingular "Greetings..." "Hello!..."
    makePage "contact" "Contact" contactLayout notDefaultPage $ do
        contactForm'
