{-# LANGUAGE OverloadedStrings #-}

module Html.Common.Card where

import Control.Monad
import Data.Maybe
import Data.NonEmpty               qualified as NE
import Data.String
import Data.Text (Text)
import Html.Common.GitHub          as GH
import Html.Common.Link
import Text.Blaze.Html5            as H hiding (main)
import Text.Blaze.Html5.Attributes as A

card ∷ AttributeValue → Text → Html → AttributeValue → Html
card cardImage cardTitle cardText cardLink = (H.div ! class_ "card col-md-4 col-12 text-center") . extLink cardLink . (H.div ! class_ "card-body") $ (do
    img ! class_ "card-img-top" ! src cardImage ! alt (textValue cardTitle) ! A.title (textValue cardTitle)
    h4 ! class_ "card-title" $ text cardTitle
    p ! class_ "card-text" $ cardText)

cardDefunct ∷ Html → Html → Html
cardDefunct cardTitle cardText = (H.div ! class_ "card col-md-4 col-12 text-center") . (H.div ! class_ "card-body") $ (do
    img ! class_ "card-img-top" ! src "img/sample.png" ! alt "Placeholder for defunct website" ! A.title "Placeholder for defunct website"
    h4 ! class_ "card-title" $ cardTitle
    p ! class_ "card-text" $ cardText)

licenceLink ∷ Licence → Html
licenceLink licence' =  a ! href ("https://spdx.org/licenses/" <> textValue (NE.getNonEmpty avOrHtmlSpdx) <> ".html") ! target "_blank" ! rel "noreferrer" $ text (NE.getNonEmpty avOrHtmlSpdx)
    where avOrHtmlSpdx = GH.spdx_id licence'

renderCard ∷ Repo → Html
renderCard repo =
    (H.div ! class_ "card col-md-4 col-12 text-center") . (H.div ! class_ "card-body") $ (do
        img ! class_ "card-img-top-github" ! A.src (fromString . show . languageToURI . language $ repo) ! A.alt (textValue . displayLanguage . language $ repo) ! A.title (textValue . displayLanguage . language $ repo)
        h4 ! class_ "card-title" $ do
            (H.span ! class_ "name") . text . NE.getNonEmpty . GH.name $ repo
            when (stars repo > 0) $ do
                " "
                H.span ! class_ "stars" $ "(" <> (fromString . show . stars $ repo) <> "★)"
            when (fork repo) . (H.span ! class_ "fork") $ "⑂"
        p ! class_ "card-text" $ do
            (H.span ! class_ "description") . text . NE.getNonEmpty . fromMaybe (NE.trustedNonEmpty "No description yet given") $ GH.description repo
            br
            maybe (small $ em "Not yet licenced") licenceLink (licence repo)
        maybe "" (\src' -> a ! class_ "btn btn-secondary mx-1" ! href (fromString (show src')) ! target "_blank" ! rel "noreferrer" $ "Source") (GH.source repo)
        maybe "" (\site -> a ! class_ "btn btn-secondary mx-1" ! href (fromString (show site)) ! target "_blank" ! rel "noreferrer" $ "Website") $ website repo)
