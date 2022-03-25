{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE UnicodeSyntax     #-}

module Html.DanDart.Page.About where

import           Control.Monad.Reader
import           Data.Env.Types
import           Html.Common.Page
import           Text.Blaze.Html5     as H hiding (main)

pageAbout ∷ (MonadReader Website m) ⇒ m Html
pageAbout = makePage "about" "About" defaultLayout notDefaultPage $ do
    p "This website entailed a few design and code decisions which I would like to explain."
    p mempty
    p $ do
        strong "The layout"
        " was based on Bootstrap. I kept the header component and chose to be without a footer component. The menus are actually a hack, such that the chosen menu item is linked to a hidden radio button which chose which sub-page to show, rather than using JS for the menu."
    p mempty
    p $ do
        strong "The code"
        " actually contains no client-side JS at all, therefore, also adding to the preference of more and more users these days to not have tracking. The website code is compiled using a custom Haskell-based codebase based on Blaze, and uploaded to GitHub Pages."
    p mempty
    p $ do
        strong "The font choice"
        " was difficult to make, as I was (and am still not quite happy enough with it, and so therefore still am) looking for a suitable, free software natural sans-serif font style, which has the single-storey \"a\", non-looped \"g\", and the double-seriffed I and J amongst other things. For now I've settled on Caudex, which whilst it is still serif, seems to be the closest I've yet to come across."
