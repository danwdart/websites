{-# LANGUAGE OverloadedStrings #-}

module Html.MadHacker.Page.Reviews where

import Control.Monad.Reader
import Data.Env.Types
import Html.Common.Bootstrap
import Html.Common.Page
import Text.Blaze.Html5            as H
import Text.Blaze.Html5.Attributes as A

pageReviews ∷ MonadReader Website m ⇒ Html → Html → Html → m Html
pageReviews reviewLinks reviewTags reviews = makePage "reviews" "Reviews" customLayout defaultPage $ do
    row $ do
        H.aside ! class_ "col-lg-2 py-1 mb-0 order-0" $ reviewLinks
        H.main ! class_ "col-lg-8 py-2 mb-0 bg-page order-lg-1 order-2" $ reviews
        H.aside ! class_ "col-lg-2 py-1 mb-1 order-lg-2 order-1" $ reviewTags
