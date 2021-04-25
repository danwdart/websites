{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE UnicodeSyntax     #-}

module Html.Common.CSS (commonCSS) where

import           Text.Blaze.Html5            as H hiding (main)
import           Text.Blaze.Html5.Attributes as A

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
    extCSS "https://stackpath.bootstrapcdn.com/bootstrap/4.4.1/css/bootstrap.min.css" "sha384-Vkoo8x4CGsO3+Hhxv8T/Q5PaXtkKtu6ug5TOeNV6gBiFeWPGFN9MuhOf23Q9Ifjh"
    css "https://cdnjs.cloudflare.com/ajax/libs/font-awesome/5.12.0-1/css/all.min.css"
    css "https://fonts.googleapis.com/css?family=Caudex"
    css "https://fonts.googleapis.com/css?family=Lexend+Deca"
    css "/css/jolharg-theme.css"
    css "/css/style.css"
