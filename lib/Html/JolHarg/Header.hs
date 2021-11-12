{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE UnicodeSyntax     #-}

module Html.JolHarg.Header where

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
import           Html.JolHarg.Page.Contact
import           Html.JolHarg.Page.FreeSoftware
import           Html.JolHarg.Page.Portfolio
import           Text.Blaze.Html5               as H hiding (main)
import           Text.Blaze.Html5.Attributes    as A

-- Todo Technologies, Pricing, Blog, About
htmlHeader ∷ Reader [Repo] (WebsiteM Html)
htmlHeader = do
    pageFs' <- pageFs
    pure $ do
        pagePortfolio' <- pagePortfolio
        pageContact' <- pageContact
        pageFs'' <- pageFs'
        pure . makeHeader "" "" mempty $ do
            pagePortfolio'
            pageFs''
            pageContact'
