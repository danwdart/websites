{-# LANGUAGE OverloadedStrings #-}

module Html.Common.Contact where

import Control.Monad.Reader
import Data.Env.Types
import Text.Blaze.Html5            as H
import Text.Blaze.Html5.Attributes as A

emailHelpSingular, emailHelpPlural ∷ Html
emailHelpSingular = "I'll never share your email with anyone else."
emailHelpPlural = "We'll never share your email with anyone else."

contactForm ∷ (MonadReader Website m) ⇒ AttributeValue → Html → AttributeValue → AttributeValue → m Html
contactForm email' _ _ _ = pure $ do
    "This form has been temporarily removed, so please "
    a ! href ("mailto:" <> email') $ "send a direct email"
    "."
