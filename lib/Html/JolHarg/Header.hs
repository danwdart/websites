{-# LANGUAGE OverloadedStrings #-}

module Html.JolHarg.Header where

import Control.Lens
import Control.Monad.Reader
import Data.Env.Types
import Data.Text.Encoding
import Html.Common.GitHub
import Html.Common.Header
import Html.Common.Page
import Html.JolHarg.Page.Contact
import Html.JolHarg.Page.FreeSoftware
import Html.JolHarg.Page.Portfolio
import Text.Blaze.Html5               as H hiding (main)

linkBlogJolHarg ∷ (MonadReader Website m) ⇒ m Html
linkBlogJolHarg = do
    urlBlogJolHarg' <- view $ urls . urlBlogJolHarg
    pure $ extNav (textValue $ decodeUtf8 urlBlogJolHarg') "Blog"

-- Todo Technologies, Pricing, Blog, About
htmlHeader ∷ (MonadReader [Repo] n, MonadReader Website m) ⇒ n (m Html)
htmlHeader = do
    pageFs' <- pageFs
    pure $ do
        pagePortfolio' <- pagePortfolio
        pageContact' <- pageContact
        pageFs'' <- pageFs'
        linkBlogJolHarg' <- linkBlogJolHarg
        pure . makeHeader "" "" mempty $ do
            pagePortfolio'
            pageFs''
            linkBlogJolHarg'
            pageContact'
