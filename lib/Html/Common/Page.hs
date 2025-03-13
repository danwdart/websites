{-# LANGUAGE OverloadedStrings #-}

module Html.Common.Page where

import Control.Lens
import Control.Monad.Reader
import Data.Env.Types
import Data.Foldable
import Data.NonEmpty               qualified as NE
import Data.String                 (IsString (fromString))
import Html.Common.Bootstrap
import Html.Common.Link
import Text.Blaze.Html5            as H
import Text.Blaze.Html5.Attributes as A

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
    (H.div ! class_ "col-md-8 offset-md-2 py-3 mb-3 bg-page")

contactLayout ∷ Html → Html
contactLayout = row .
    (H.div ! class_ "col-lg-6 offset-lg-3 col-sm-12 col-md-12 col-xs-12 bg-page p-3 mb-3")

customLayout ∷ Html → Html
customLayout = Prelude.id

renderBreadcrumb ∷ MonadReader Website m ⇒ m Html
renderBreadcrumb = do
    breadcrumb' <- view breadcrumb
    pure . small $ foldMap' (\(label', mUrl) -> do
        "» "
        maybe (text (NE.getNonEmpty label')) (\url -> a ! href (stringValue $ show url) $ text (NE.getNonEmpty label')) mUrl
        " "
        ) (getBreadcrumb breadcrumb')

makePage ∷ (MonadReader Website m) ⇒ AttributeValue → String → (Html → Html) → Attribute → Html → m Html
makePage pageId label' layout extraParams content' = do
    breadcrumb' <- renderBreadcrumb
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
                    (H.div ! class_ "col my-lg-3 my-md-5 my-sm-2 my-xs-3") $
                        breadcrumb'
                layout content'
