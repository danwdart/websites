{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE UnicodeSyntax     #-}

module Html.Common.Contact (contactForm, emailHelpSingular, emailHelpPlural) where

import Data.Env
import           Text.Blaze.Html5            as H
import           Text.Blaze.Html5.Attributes as A

emailHelpSingular, emailHelpPlural ∷ Html
emailHelpSingular = "I'll never share your email with anyone else."
emailHelpPlural = "We'll never share your email with anyone else."

contactForm ∷ AttributeValue → Html → AttributeValue → AttributeValue → WebsiteM Html
contactForm email emailHelpMessage summaryPlaceholder messagePlaceholder = do
    endpoint' <- asks endpoint
    pure . (H.form
        ! class_ "form"
        ! enctype "application/x-www-form-urlencoded"
        ! action (textValue endpoint' <> "/contact?email=" <> email)
        ! method "post"
        ! target "_result") $ do
        H.div ! class_ "form-group" $ do
            H.label ! for "name" $ "Your name"
            input ! class_ "form-control" ! A.id "name" ! type_ "text" ! placeholder "John Smith" ! name "name" ! autocomplete "name"
        H.div ! class_ "form-group" $ do
            H.label ! for "email" $ "Your email"
            input ! class_ "form-control" ! A.id "email" ! type_ "email" ! placeholder "john@smith.com" ! name "email" ! autocomplete "email"
            small ! class_ "form-text text-muted" ! A.id "emailHelp" $ emailHelpMessage
        H.div ! class_ "form-group" $ do
            H.label ! for "subject" $ "Summary"
            input ! class_ "form-control" ! A.id "subject" ! type_ "text" ! placeholder summaryPlaceholder ! name "subject"
        H.div ! class_ "form-group" $ do
            H.label ! for "message" $ "Your message"
            textarea ! class_ "form-control" ! A.id "message" ! placeholder messagePlaceholder ! rows "10" ! name "message" $ mempty
        H.div ! class_ "form-group" $ do
            input ! class_ "btn btn-primary" ! type_ "submit" ! value "Send"
            H.iframe ! name "_result" ! height "90" ! width "300" ! A.style "border: 0; vertical-align: middle; margin-left: 10px;" $ mempty
