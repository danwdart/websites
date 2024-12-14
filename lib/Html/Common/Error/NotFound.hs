{-# LANGUAGE OverloadedStrings #-}

module Html.Common.Error.NotFound where

import Control.Monad.Reader
import Data.Env.Types              as Env
import Html.Common.Bootstrap
import Html.Common.Head
import Text.Blaze.Html5            as H
import Text.Blaze.Html5.Attributes as A

defaultPage404 ∷ MonadReader Website m => m Html
defaultPage404 = do
    head' <- htmlHead
    pure . (docTypeHtml ! lang "en-GB") $ do
        head'
        body $ do
            row . (H.div ! class_ "col-12 text-center") $ do
                h1 "Oh no!"
                p "Sorry, I couldn't find that."
                p . (a ! href "/") $ "Back to main page"
