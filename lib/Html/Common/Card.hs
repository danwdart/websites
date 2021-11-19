{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE UnicodeSyntax     #-}

module Html.Common.Card where

import           Control.Monad
import           Data.Maybe
import           Data.String
import           Html.Common.GitHub          as GH
import           Html.Common.Link
import           Text.Blaze.Html5            as H hiding (main)
import           Text.Blaze.Html5.Attributes as A

genericImage ∷ AttributeValue
genericImage = "https://web.archive.org/web/20181125122112if_/https://upload.wikimedia.org/wikipedia/commons/1/1a/Code.jpg"

imagesFs ∷ [(Language, AttributeValue)]
imagesFs = [
    (LangASM, "https://upload.wikimedia.org/wikipedia/commons/thumb/f/f3/Motorola_6800_Assembly_Language.png/800px-Motorola_6800_Assembly_Language.png"),
    (LangBlitzBasic, "https://upload.wikimedia.org/wikipedia/en/6/65/BlitzBasicLogo.gif"),
    (LangC, "https://upload.wikimedia.org/wikipedia/commons/3/3b/C.sh-600x600.png"),
    (LangCoffee, "https://farm8.staticflickr.com/7212/7168325292_16a46a1fea_n.jpg"),
    (LangCPP, "https://upload.wikimedia.org/wikipedia/commons/5/5c/Images_200px-ISO_C%2B%2B_Logo_svg.png"),
    (LangDocker, "https://www.docker.com/sites/default/files/d8/2019-07/vertical-logo-monochromatic.png"),
    (LangGeneric, genericImage),
    (LangHS, "https://upload.wikimedia.org/wikipedia/commons/thumb/1/1c/Haskell-Logo.svg/1280px-Haskell-Logo.svg.png"),
    (LangHTML, "https://upload.wikimedia.org/wikipedia/commons/thumb/6/61/HTML5_logo_and_wordmark.svg/512px-HTML5_logo_and_wordmark.svg.png"),
    (LangJS, "https://upload.wikimedia.org/wikipedia/commons/6/6a/JavaScript-logo.png"),
    (LangNix, "https://nixos.wiki/images/thumb/2/20/Home-nixos-logo.png/x207px-Home-nixos-logo.png.pagespeed.ic.38jujIAhx5.png"),
    (LangPHP, "https://upload.wikimedia.org/wikipedia/commons/thumb/2/27/PHP-logo.svg/711px-PHP-logo.svg.png"),
    (LangPython, "https://upload.wikimedia.org/wikipedia/commons/0/0a/Python.svg"),
    (LangShell, "https://upload.wikimedia.org/wikipedia/commons/thumb/0/08/Antu_bash.svg/512px-Antu_bash.svg.png"),
    (LangTcl, "https://upload.wikimedia.org/wikipedia/commons/4/41/Tcl.svg"),
    (LangTS, "https://rynop.files.wordpress.com/2016/09/ts.png?w=200"),
    (LangVB, "https://upload.wikimedia.org/wikipedia/en/e/e4/Visual_Basic_6.0_logo.png")
    ]

languageImage ∷ Language → AttributeValue
languageImage l = fromMaybe genericImage (lookup l imagesFs)

card ∷ AttributeValue → Html → Html → AttributeValue → Html
card cardImage cardTitle cardText cardLink = (H.div ! class_ "card col-md-4 col-12 text-center") . extLink cardLink . (H.div ! class_ "card-body") $ (do
    img ! class_ "card-img-top" ! src cardImage
    h4 ! class_ "card-title" $ cardTitle
    p ! class_ "card-text" $ cardText)

cardDefunct ∷ Html → Html → Html
cardDefunct cardTitle cardText = (H.div ! class_ "card col-md-4 col-12 text-center") . (H.div ! class_ "card-body") $ (do
    img ! class_ "card-img-top" ! src "img/sample.png"
    h4 ! class_ "card-title" $ cardTitle
    p ! class_ "card-text" $ cardText)

licenceLink ∷ Licence → Html
licenceLink licence' =  a ! href ("https://spdx.org/licenses/" <> fromString avOrHtmlSpdx <> ".html") ! target "_blank" $ fromString avOrHtmlSpdx
    where avOrHtmlSpdx = GH.spdx_id licence'

renderCard ∷ Repo → Html
renderCard repo =
    (H.div ! class_ "card col-md-4 col-12 text-center") . (H.div ! class_ "card-body") $ (do
        img ! class_ "card-img-top-github" ! A.src (languageImage . language $ repo)
        h4 ! class_ "card-title" $ do
            (H.span ! class_ "name") . fromString . GH.name $ repo
            " "
            H.span ! class_ "stars" $ "(" <> (fromString . show . stars $ repo) <> "★)"
            when (fork repo) . (H.span ! class_ "fork") $ "⑂"
        p ! class_ "card-text" $ do
            (H.span ! class_ "description") . fromString . fromMaybe "No description yet given" $ GH.description repo
            br
            maybe (small $ em "Not yet licenced") licenceLink (licence repo)
        maybe "" (\src' -> a ! class_ "btn btn-secondary mx-1" ! href (fromString src') ! target "_blank" $ "Source") (GH.source repo)
        maybe "" (\site -> a ! class_ "btn btn-secondary mx-1" ! href (fromString site) ! target "_blank" $ "Website") $ website repo)
