{-# LANGUAGE OverloadedStrings #-}

module Html.JolHarg.Page.Portfolio where

import Control.Lens
import Control.Monad.Reader
import Data.Env.Types
import Data.Text.Encoding          qualified as TE
import Data.NonEmpty               qualified as NE
import Html.Common.Bootstrap
import Html.Common.Card
import Html.Common.Link
import Html.Common.Page
import Text.Blaze.Html5            as H hiding (main)
import Text.Blaze.Html5.Attributes as A
import Text.Email.Parser

pagePortfolio ∷ (MonadReader Website m) ⇒ m Html
pagePortfolio = do
    email' <- view email
    plainBreadcrumb (NE.trustedNonEmpty "Portfolio") . makePage "portfolio" "Portfolio" customLayout defaultPage $ do
        row . (H.div ! class_ "col-md-12 text-center") $
            p "Some of the websites, projects and companies Dan Dart has been involved with are:"
        row $ do
            (H.div ! class_ "card col-md-4 col-12 text-center") . extLink ("mailto:" <> textValue (TE.decodeUtf8Lenient (toByteString email'))) . (H.div ! class_ "card-body") $ (do
                img ! class_ "card-img-top" ! src "img/sample.png"
                h4 ! class_ "card-title" $ "Your website here"
                p ! class_ "card-text" $ "Make an enquiry for a website or project.")
            card "img/equalsmoney.png" "Equals Money" "B2B money management platform" "https://equalsmoney.com"
            card "img/fairfx.png" "FairFX" "Currency card and conversion" "https://fairfx.com"
            card "img/roqqett.png" "Roqqett" "Fast checkout app and website plugin" "https://roqqett.com"
            card "img/timezap.png" "TimeZap" "Set-and-forget accurate time tracking" "https://timezap.ai"
            card "img/eppiq.png" "Eppiq Marketing" "Digital marketing agency" "https://eppiq.co.uk"
            card "img/penta.png" "Penta Consulting" "Web Agency" "https://www.pentaconsulting.com/"
            card "img/dotfive.png" "DotFive" "Promotional website" "https://www.dotfive.co.uk/"
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
            card "img/cloudbanter.png" "Cloudbanter" "Mobile operator messaging system" "https://cloudbanter.com/"
            card "img/reviverest.png" "Revive Ad server REST API" "RESTful API for Open source ad server" "https://www.reviveadserverrestapi.com/"
            cardDefunct "ThemeAttic" "Inventors' search platform"
            card "img/soampli.png" "SoAmpli" "Social media amplification" "https://www.soampli.com/"
            card "img/viewex.png" "Viewex" "Advertising revenue optimisation" "https://viewex.co.uk"
            card "img/canddi.png" "CANDDi" "Smart web analytics" "https://canddi.com"
            card "img/mobilefun.png" "Mobile Fun" "Web shop for phones and accessories" "https://mobilefun.co.uk"
            -- gamingzap now redirects to mobilefun
            card "img/gamingzap.png" "Gamingzap" "Web shop for gaming devices and accessories" "https://www.mobilefun.co.uk/"
            card "img/gearzap.png" "Gearzap" "Web shop for cases and accessories" "https://gearzap.com"
            card "img/rattray.png" "Rattray Mosaics" "Portfolio and personal website of local mosaic artist" "https://rattraymosaics.co.uk"
            card "img/smdaf.png" "Shepton Mallet Digital Arts Festival" "Local festival site" "https://sheptondigitalarts.co.uk"
            card "img/ssoha.png" "SSOHA" "Somerset School of Oriental Healing Arts" "https://ssoha.org.uk"
