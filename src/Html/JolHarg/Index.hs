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
    row . (H.div ! class_ "col-md-12 text-center") $ p "Some of the websites and projects JolHarg Ltd has been involved with are:"
    row $ do
        (H.div ! class_ "card col-md-4 text-center") . (H.div ! class_ "card-body") $ (do
            img ! class_ "card-img-top" ! src "img/sample.png"
            h4 ! class_ "card-title" $ "You"
            p ! class_ "card-text" $ "Make an enquiry for a website or project:"
            extLink "mailto:website@jolharg.com" ! class_ "btn btn-primary" $ "Enquire")
        card "img/techradar.png" "TechRadar" "Technology website" "https://techradar.com/"
        card "img/feeld.png" "Feeld" "Dating app for singles and couples" "https://feeld.co/"
        card "img/polaris.png" "Polaris Elements" "Hospitality software" "https://polaris-elements.co.uk/"
        cardDefunct "Plugin ASO" "Analytics dashboard for Shopify"
        card "img/faultfixers.png" "FaultFixers" "Facilities management" "https://faultfixers.com"
        card "img/dadi.png" "DADI" "DADI web services suite" "https://dadi.cloud/en/"
        card "img/planetradio.png" "Planet Radio" "Collection of UK radio magazine websites" "https://planetradio.co.uk/"
        card "img/kompli.png" "Kompli Global" "Due diligence and search intelligence" "https://kompli-global.com"
        card "img/canddi.png" "CANDDi" "Smart web analytics" "https://canddi.com"
        card "img/cloudbanter.png" "Cloudbanter" "Mobile operator messaging system" "http://cloudbanter.com/"
        card "img/reviverest.png" "Revive Ad server REST API" "RESTful API for Open source ad server" "https://www.reviveadserverrestapi.com/"
        cardDefunct "ThemeAttic" "Inventors' search platform"
        card "img/soampli.png" "SoAmpli" "Social media amplification" "https://soampli.com"
        card "img/viewex.png" "Viewex" "Advertising revenue optimisation" "http://viewex.co.uk"
        card "img/mobilefun.png" "Mobile Fun" "Web shop for phones and accessories" "http://mobilefun.co.uk"
        card "img/gamingzap.png" "Gamingzap" "Web shop for gaming devices and accessories" "http://gamingzap.com"
        card "img/gearzap.png" "Gearzap" "Web shop for cases and accessories" "http://gearzap.com"
        card "img/rattray.png" "Rattray Mosaics" "Portfolio and personal website of local mosaic artist" "http://rattraymosaics.co.uk"
        card "img/smdaf.png" "Shepton Mallet Digital Arts Festival" "Local festival site" "http://sheptondigitalarts.co.uk"
        card "img/ssoha.png" "SSOHA" "Somerset School of Oriental Healing Arts" "http://ssoha.org.uk"

pageFs ∷ Reader [Repo] (WebsiteM Html)
pageFs = do
    repos <- ask
    pure . makePage "fs" "Free Software" customLayout notDefaultPage $ do
        row . (H.div ! class_ "col-md-12 text-center") $ p "Some of the free software projects JolHarg Ltd has created or contributed to are:"
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
