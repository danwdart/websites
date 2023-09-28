{-# LANGUAGE OverloadedStrings #-}

module Html.M0ORI.Page.Contact where

import Control.Monad.Reader
import Data.Env.Types
import Html.Common.Contact
import Html.Common.Page
import Text.Blaze.Html5     as H hiding (main)

pageContact ∷ MonadReader Website m ⇒ m Html
pageContact = do
    contactForm' <- contactForm "website@m0ori.com" emailHelpSingular "Greetings..." "Hello!..."
    makePage "contact" "Contact" contactLayout notDefaultPage $ do
        contactForm'
