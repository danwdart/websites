{-# LANGUAGE OverloadedStrings #-}

module Html.M0ORI.Index where

import Control.Monad.Reader
import Data.Env.Types              as Env
import Html.Common.Error.NotFound
import Html.Common.Head
import Html.M0ORI.Header
import Text.Blaze.Html5            as H hiding (main)
import Text.Blaze.Html5.Attributes as A

page ∷ MonadReader Website m ⇒ m Html
page = do
    header' <- htmlHeader
    head' <- htmlHead
    pure . (docTypeHtml ! lang "en-GB") $ do
        head'
        header'

page404 ∷ MonadReader Website m ⇒ m Html
page404 = defaultPage404
