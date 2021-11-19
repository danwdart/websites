{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE UnicodeSyntax     #-}

module Html.JolHarg.Page.Portfolio where

import           Control.Monad.Reader
import           Data.Env
import           Data.Site.JolHarg
import           Html.Common.Bootstrap
import           Html.Common.Card
import           Html.Common.Contact
import           Html.Common.Error.NotFound
import           Html.Common.Head
import           Html.Common.Header
import           Html.Common.Link
import           Html.Common.Page
import           Text.Blaze.Html5            as H hiding (main)
import           Text.Blaze.Html5.Attributes as A

pagePortfolio ∷ WebsiteM Html
pagePortfolio = makePage "portfolio" "Portfolio" customLayout defaultPage $ do
    row . (H.div ! class_ "col-md-12 text-center") $
        p "Some of the websites, projects and companies JolHarg Ltd has been involved with are:"
    row $ do
        (H.div ! class_ "card col-md-4 col-12 text-center") . extLink "mailto:website@jolharg.com" . (H.div ! class_ "card-body") $ (do
            img ! class_ "card-img-top" ! src "img/sample.png"
            h4 ! class_ "card-title" $ "You"
            p ! class_ "card-text" $ "Make an enquiry for a website or project.")
        card "img/timezap.png" "TimeZap" "Set-and-forget accurate time tracking" "https://timezap.ai"
        card "img/eppiq.png" "Eppiq Marketing" "Digital marketing agency" "https://eppiq.co.uk"
        card "img/penta.png" "Penta Consulting" "Web Agency" "https://www.pentaconsulting.com/"
        card "img/dotfive.png" "DotFive" "Promotional website" "https://dotfive.co.uk"
        card "img/usaycompare.png" "Usaycompare" "Health insurance comparison" "https://usaycompare.co.uk"
        card "img/future.png" "Future Publishing" "Technology publication" "https://futureplc.com"
        card "img/techradar.png" "TechRadar" "Technology website" "https://techradar.com/"
        card "img/feeld.png" "Feeld" "Dating app for singles and couples" "https://feeld.co/"
        card "img/polaris.png" "Polaris Elements" "Hospitality software" "https://polaris-elements.co.uk/"
        cardDefunct "Plugin ASO" "Analytics dashboard for Shopify"
        card "img/faultfixers.png" "FaultFixers" "Facilities management" "https://faultfixers.com"
        -- https://dadi.cloud/en/ is currently broken
        card "img/dadi.png" "DADI" "DADI web services suite" "https://docs.dadi.cloud/"
        card "img/planetradio.png" "Planet Radio" "Collection of UK radio magazine websites" "https://planetradio.co.uk/"
        card "img/kompli.png" "Kompli Global" "Due diligence and search intelligence" "https://kompli-global.com"
        card "img/canddi.png" "CANDDi" "Smart web analytics" "https://canddi.com"
        card "img/cloudbanter.png" "Cloudbanter" "Mobile operator messaging system" "http://cloudbanter.com/"
        card "img/reviverest.png" "Revive Ad server REST API" "RESTful API for Open source ad server" "https://www.reviveadserverrestapi.com/"
        cardDefunct "ThemeAttic" "Inventors' search platform"
        card "img/soampli.png" "SoAmpli" "Social media amplification" "https://soampli.com"
        card "img/viewex.png" "Viewex" "Advertising revenue optimisation" "http://viewex.co.uk"
        card "img/mobilefun.png" "Mobile Fun" "Web shop for phones and accessories" "http://mobilefun.co.uk"
        -- gamingzap now redirects to mobilefun
        card "img/gamingzap.png" "Gamingzap" "Web shop for gaming devices and accessories" "https://www.mobilefun.co.uk/"
        card "img/gearzap.png" "Gearzap" "Web shop for cases and accessories" "http://gearzap.com"
        card "img/rattray.png" "Rattray Mosaics" "Portfolio and personal website of local mosaic artist" "http://rattraymosaics.co.uk"
        card "img/smdaf.png" "Shepton Mallet Digital Arts Festival" "Local festival site" "http://sheptondigitalarts.co.uk"
        card "img/ssoha.png" "SSOHA" "Somerset School of Oriental Healing Arts" "http://ssoha.org.uk"
