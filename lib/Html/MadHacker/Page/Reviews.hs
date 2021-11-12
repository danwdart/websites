{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE UnicodeSyntax     #-}

module Html.MadHacker.Page.Reviews where

import           Data.Env
import           Data.Site.MadHacker
import           Html.Common.Bootstrap
import           Html.Common.Error.NotFound
import           Html.Common.Head
import           Html.Common.Page
import           Text.Blaze.Html5            as H hiding (main)
import           Text.Blaze.Html5.Attributes as A

pageReviews ∷ Html → Html → WebsiteM Html
pageReviews reviewLinks reviews = makePage "reviews" "Reviews" customLayout defaultPage $ do
    row $ do
        H.div ! class_ "col-md-2 py-3 mb-3" $ reviewLinks
        H.div ! class_ "col-md-8 py-3 mb-3 bg-light" $ reviews
