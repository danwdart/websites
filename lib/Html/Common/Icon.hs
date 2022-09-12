{-# LANGUAGE OverloadedStrings #-}

module Html.Common.Icon where

import           Data.String
import           Data.Text                   (Text)
import           Text.Blaze.Html5            as H hiding (main)
import           Text.Blaze.Html5.Attributes as A

data IconType = B | S
instance Show IconType where
    show B = "b"
    show S = "s"

icon ∷ IconType → Text → Html
icon iconType iconName = (i ! class_ ("fa" <> fromString (show iconType) <> " fa-" <> textValue iconName)) mempty
