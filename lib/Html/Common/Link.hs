{-# LANGUAGE OverloadedStrings #-}

module Html.Common.Link where

import Text.Blaze.Html5            as H hiding (main)
import Text.Blaze.Html5.Attributes as A

extLink ∷ AttributeValue → Html → Html
extLink linkHref = a ! href linkHref ! target "_blank" ! rel "noopener"

extLinkTitle ∷ AttributeValue → AttributeValue → Html → Html
extLinkTitle linkHref linkTitle = extLink linkHref ! A.title linkTitle
