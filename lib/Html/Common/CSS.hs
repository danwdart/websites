{-# LANGUAGE OverloadedStrings #-}

module Html.Common.CSS where

import Text.Blaze.Html5            as H hiding (main)
import Text.Blaze.Html5.Attributes as A

css ∷ AttributeValue → Html
css sourceUrl = link ! rel "stylesheet" ! type_ "text/css" ! href sourceUrl ! customAttribute "crossorigin" "anonymous"

extCSS ∷ AttributeValue → AttributeValue → Html
extCSS sourceUrl integrityHash = link ! rel "stylesheet" ! type_ "text/css" ! href sourceUrl ! customAttribute "integrity" integrityHash ! customAttribute "crossorigin" "anonymous"

commonCSS ∷ Html
commonCSS = do
    meta ! charset "utf-8"
    meta ! name "viewport" ! content "width=device-width, initial-scale=1"
    meta ! name "theme-color" ! content "#800080"
    meta ! name "rating" ! content "General"
    meta ! name "referrer" ! content "no-referrer"
    link ! rel "license" ! href "LICENSE"
    link ! rel "shortcut icon" ! type_ "image/png" ! href "/img/favicon.png"
    link ! rel "author" ! type_ "text/plain" ! href "/humans.txt"
    extCSS "https://cdn.jsdelivr.net/npm/bootstrap@5.1.1/dist/css/bootstrap.min.css" "sha384-F3w7mX95PdgyTmZZMECAngseQB83DfGTowi0iMjiWaeVhAn4FJkqJByhZMI3AhiU"
    css "https://cdnjs.cloudflare.com/ajax/libs/font-awesome/5.15.4/css/all.min.css"
    css "https://fonts.googleapis.com/css?family=Caudex"
    css "https://fonts.googleapis.com/css?family=Lexend+Deca"
    css "/css/jolharg-theme.css"
    css "/css/style.css"
