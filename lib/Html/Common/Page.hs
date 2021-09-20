{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE UnicodeSyntax     #-}

module Html.Common.Page (customLayout, contactLayout, defaultLayout, dlNav, extNav, defaultPage, notDefaultPage, makePage) where

import           Data.Env
import           Data.String                 (IsString (fromString))
import           Html.Common.Bootstrap
import           Html.Common.Link
import           Text.Blaze.Html5            as H
import           Text.Blaze.Html5.Attributes as A

navBtn ∷ Attribute
navBtn = class_ "nav-link btn btn-sm"

extNav ∷ AttributeValue → Html → Html
extNav url' = (li ! class_ "nav-item") . (extLink url' ! navBtn)

dlNav ∷ AttributeValue → Html → Html
dlNav url' = (li ! class_ "nav-item") .
    (a
        ! navBtn
        ! href url'
        ! customAttribute "download" ""
    )

defaultPage ∷ Attribute
defaultPage = checked "checked"

notDefaultPage ∷ Attribute
notDefaultPage = mempty

defaultLayout ∷ Html → Html
defaultLayout = row .
    (H.div ! class_ "col-md-8 offset-md-2 py-3 mb-3 bg-light")

contactLayout ∷ Html → Html
contactLayout = row .
    (H.div ! class_ "col-lg-6 offset-lg-3 col-sm-12 col-md-12 col-xs-12 bg-light p-3 mb-3")

customLayout ∷ Html → Html
customLayout = Prelude.id

makePage ∷ AttributeValue → String → (Html → Html) → Attribute → Html → WebsiteM Html
makePage pageId label' layout extraParams content' = do
    pure . (li ! class_ "nav-item") $ do
        input
            ! type_ "radio"
            ! A.style "display:none"
            ! name "selected"
            ! A.id (fromString label')
            ! value (fromString label')
            ! extraParams
        (
            H.label
                ! class_ "mb-0"
                ! for (fromString label')
            ) . (
                a
                    ! navBtn
            ) $ fromString label'
        H.div
            ! class_ "page"
            ! A.id pageId $ do
                row .
                    (H.div ! class_ "col my-md-3") .
                        small $ "» " <> fromString label'
                layout content'
