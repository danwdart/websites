{-# LANGUAGE OverloadedStrings #-}

module Html.Common.Social where

import           Data.Text                   (Text)
import           Html.Common.Icon            as Icon
import           Html.Common.Link
import           Text.Blaze.Html5            as H hiding (main)
import           Text.Blaze.Html5.Attributes as A

type Href = AttributeValue
type Title = AttributeValue

link' ∷ AttributeValue → AttributeValue → Html → Html
link' linkHref linkTitle = extLinkTitle linkHref linkTitle ! class_ "social" ! A.style "color:black"

socialIcon ∷ IconType → Href → Title → Text → Html
socialIcon iconType linkHref linkTitle iconName = link' linkHref linkTitle $ Icon.icon iconType iconName

socialIconB ∷ Href → Title → Text → Html
socialIconB = socialIcon B

socialIconS ∷ Href → Title → Text → Html
socialIconS = socialIcon S
