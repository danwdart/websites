{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE UnicodeSyntax #-}

module Html.Common.Contact (contactForm, emailHelpSingular, emailHelpPlural) where

import Text.Blaze.Html5 as H
import Text.Blaze.Html5.Attributes as A

emailHelpSingular, emailHelpPlural :: Html
emailHelpSingular = "I'll never share your email with anyone else."
emailHelpPlural = "We'll never share your email with anyone else."

contactForm :: AttributeValue -> Html -> AttributeValue -> AttributeValue -> Html
contactForm email emailHelpMessage summaryPlaceholder messagePlaceholder = H.form ! action ("https://formspree.io/" <> email) ! method "post" $ do
    H.div ! class_ "form-group" $ do
        H.label ! for "name" $ "Your name"
        input ! class_ "form-control" ! A.id "name" ! type_ "text" ! placeholder "John Smith" ! name "name" ! autocomplete "name"
    H.div ! class_ "form-group" $ do
        H.label ! for "email" $ "Your email"
        input ! class_ "form-control" ! A.id "email" ! type_ "email" ! placeholder "john@smith.com" ! name "email" ! autocomplete "email"
        small ! class_ "form-text text-muted" ! A.id "emailHelp" $ emailHelpMessage
    H.div ! class_ "form-group" $ do
        H.label ! for "subject" $ "Summary"
        input ! class_ "form-control" ! A.id "subject" ! type_ "text" ! placeholder summaryPlaceholder ! name "_subject"
    H.div ! class_ "form-group" $ do
        H.label ! for "message" $ "Your message"
        textarea ! class_ "form-control" ! A.id "message" ! placeholder messagePlaceholder ! rows "10" ! name "message" $ mempty
    H.div ! class_ "form-group" $ input ! class_ "btn btn-primary" ! type_ "submit" ! value "Send"