{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE UnicodeSyntax     #-}

module Html.Common.Contact where

import           Data.Env
import           Text.Blaze.Html5            as H
import           Text.Blaze.Html5.Attributes as A

emailHelpSingular, emailHelpPlural ∷ Html
emailHelpSingular = "I'll never share your email with anyone else."
emailHelpPlural = "We'll never share your email with anyone else."

contactForm ∷ AttributeValue → Html → AttributeValue → AttributeValue → WebsiteM Html
contactForm email _ _ _ = pure $ do
    "This form has been temporarily removed, so please "
    a ! href ("mailto:" <> email) $ "send a direct email"
    "."
