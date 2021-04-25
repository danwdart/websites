{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE UnicodeSyntax     #-}

module Html.JolHarg.Index (page, page404) where

import           Control.Monad.Reader
import           Data.Env
import           Data.Site.JolHarg

import           Html.Common.Card
import           Html.Common.Head

import           Html.Common.Bootstrap
import           Html.Common.Contact
import           Html.Common.Error.NotFound
import           Html.Common.Header
import           Html.Common.Link
import           Html.Common.Page
import           Html.Common.Visit
import           Text.Blaze.Html5            as H hiding (main)
import           Text.Blaze.Html5.Attributes as A

pagePortfolio ∷ WebsiteM Html
pagePortfolio = makePage "portfolio" "Portfolio" customLayout defaultPage $ do
    row . divClass "col-md-12 text-center" $ p "Some of the websites and projects JolHarg Ltd has been involved with are:"
    row $ do
        divClass "card col-md-4 text-center" . divClass "card-body" $ (do
            img ! class_ "card-img-top" ! src "img/sample.png"
            h4 ! class_ "card-title" $ "You"
            p ! class_ "card-text" $ "Make an enquiry for a website or project:"
            extLink "mailto:website@jolharg.com" ! class_ "btn btn-primary" $ "Enquire")
      
pageFs ∷ Reader [Repo] (WebsiteM Html)
pageFs = do
    repos <- ask
    pure . makePage "fs" "Free Software" customLayout notDefaultPage $ do
        row . (divClass "col-md-12 text-center") $ p "Some of the free software projects JolHarg Ltd has created or contributed to are:"
        mapM_ renderCard repos

pageContact ∷ WebsiteM Html
pageContact = do
    contactForm' <- contactForm "website@jolharg.com" emailHelpPlural "Website for me..." "I am interested in a website..."
    makePage "contact" "Contact" contactLayout notDefaultPage $ do
        contactForm'

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

page ∷ Reader [Repo] (WebsiteM Html)
page = do
    header' <- htmlHeader
    pure $ do
        head' <- htmlHead descTitle keywords mempty
        visit' <- visit "jolharg"
        header'' <- header'
        pure . (docTypeHtml ! lang "en-GB") $ do
            head'
            header''
            visit'

page404 ∷ WebsiteM Html
page404 = do
    visit' <- visit "jolharg404"
    defaultPage404 descTitle keywords visit'
