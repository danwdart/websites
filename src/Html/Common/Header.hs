{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE UnicodeSyntax     #-}

module Html.Common.Header (makeHeader) where

import           Text.Blaze.Html5            as H
import           Text.Blaze.Html5.Attributes as A

makeHeader ∷ AttributeValue → String → Html → Html → Html
makeHeader mainLink title' extraHeaderContent content' =
    nav
    ! class_ "p-0 p-sm-2 navbar d-block d-sm-flex navbar-expand navbar-dark bg-primary" $ do
        a
            ! class_ ((if title' == "" then "w-75 " else "") <> "p-0 pt-1 pt-sm-0 w-sm-auto text-center text-sm-left navbar-brand")
            ! href mainLink $ do
                img
                    ! src "/img/header.png"
                    ! A.style "height:32px"
                    ! alt ""
                if title' == "" then "" else H.span
                    ! class_ "title ml-2" $ string title'
        H.div $ do
            ul ! class_ "navbar-nav px-3" $ content'
            extraHeaderContent
