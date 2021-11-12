{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE UnicodeSyntax     #-}

module Html.JolHarg.Page.FreeSoftware where

import           Control.Monad.Reader
import           Data.Env
import           Data.Site.JolHarg
import           Html.Common.Bootstrap
import           Html.Common.Card
import           Html.Common.Contact
import           Html.Common.Error.NotFound
import           Html.Common.GitHub
import           Html.Common.Head
import           Html.Common.Header
import           Html.Common.Link
import           Html.Common.Page
import           Text.Blaze.Html5            as H hiding (main)
import           Text.Blaze.Html5.Attributes as A

pageFs ∷ Reader [Repo] (WebsiteM Html)
pageFs = do
    repos <- ask
    pure . makePage "fs" "Free Software" customLayout notDefaultPage $ do
        row . (H.div ! class_ "col-md-12 text-center") $ p "Some of the free software projects JolHarg Ltd has created or contributed to are:"
        mapM_ renderCard repos
