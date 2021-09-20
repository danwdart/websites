{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE UnicodeSyntax     #-}

module Html.Common.Social (socialIconB, socialIconS) where

import           Data.String
import           Html.Common.Link
import           Text.Blaze.Html5            as H hiding (main)
import           Text.Blaze.Html5.Attributes as A

type Href = AttributeValue
type Title = AttributeValue
type IconName = AttributeValue

data IconType = B | S
instance Show IconType where
    show B = "b"
    show S = "s"

link' ∷ AttributeValue → AttributeValue → Html → Html
link' linkHref linkTitle = extLinkTitle linkHref linkTitle ! class_ "social" ! A.style "color:black"

socialIcon ∷ IconType → Href → Title → IconName → Html
socialIcon iconType linkHref linkTitle iconName = link' linkHref linkTitle .
    (i ! class_ ("fa" <> fromString (show iconType) <> " fa-" <> iconName)) $ mempty

socialIconB ∷ Href → Title → IconName → Html
socialIconB = socialIcon B

socialIconS ∷ Href → Title → IconName → Html
socialIconS = socialIcon S
