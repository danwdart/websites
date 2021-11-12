{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE UnicodeSyntax     #-}

module Html.DanDart.Page.Contact where

import           Data.Env
import           Data.Site.DanDart
import           Html.Common.Audio
import           Html.Common.Contact
import           Html.Common.Error.NotFound
import           Html.Common.Head
import           Html.Common.Header
import           Html.Common.Link
import           Html.Common.Page
import           Html.Common.Shortcuts
import           Html.Common.Social
import           Text.Blaze.Html5            as H hiding (main)
import           Text.Blaze.Html5.Attributes as A

pageContact ∷ WebsiteM Html
pageContact = do
    contactForm' <- contactForm "website@dandart.co.uk" emailHelpSingular "Greetings..." "Hello!..."
    makePage "contact" "Contact" contactLayout notDefaultPage $ do
        contactForm'