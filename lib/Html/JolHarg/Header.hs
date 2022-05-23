{-# LANGUAGE OverloadedStrings #-}

module Html.JolHarg.Header where

import           Control.Monad.Reader
import           Data.Env.Types
import           Html.Common.GitHub
import           Html.Common.Header
import           Html.JolHarg.Page.Contact
import           Html.JolHarg.Page.FreeSoftware
import           Html.JolHarg.Page.Portfolio
import           Text.Blaze.Html5               as H hiding (main)

-- Todo Technologies, Pricing, Blog, About
htmlHeader ∷ (MonadReader [Repo] n, MonadReader Website m) ⇒ n (m Html)
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
