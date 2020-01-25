{-# LANGUAGE OverloadedStrings #-}

module Html.Common.Social (socialIconB, socialIconS, socialIconBBanned, socialIconSBanned) where

import Text.Blaze.Html5 as H hiding (main)
import Text.Blaze.Html5.Attributes as A

type Href = AttributeValue
type Title = AttributeValue
type IconName = AttributeValue

socialIconB :: Href -> Title -> IconName -> Html
socialIconB linkHref linkTitle iconName = a ! class_ "social" ! href linkHref ! A.style "color:black" ! A.title linkTitle ! target "_blank" ! rel "noopener" $ i ! class_ ("fab fa-" <> iconName) $ mempty

socialIconS :: Href -> Title -> IconName -> Html
socialIconS linkHref linkTitle iconName = a ! class_ "social" ! href linkHref ! A.style "color:black" ! A.title linkTitle ! target "_blank" ! rel "noopener" $ i ! class_ ("fas fa-" <> iconName) $ mempty

socialIconBBanned :: Href -> Title -> IconName -> Html
socialIconBBanned linkHref linkTitle iconName = a ! class_ "social" ! href linkHref ! A.style "color:black" ! A.title linkTitle ! target "_blank" ! rel "noopener" $
    H.span ! class_ "fa-stack fa-1x" ! A.style "font-size: 0.5em; height: 2.6em" $ do
        i ! class_ ("fab fa-stack-1x fa-" <> iconName) $ mempty
        i ! class_ "fas fa-ban fa-stack-2x" $ mempty

socialIconSBanned :: Href -> Title -> IconName -> Html
socialIconSBanned linkHref linkTitle iconName = a ! class_ "social" ! href linkHref ! A.style "color:black" ! A.title linkTitle ! target "_blank" ! rel "noopener" $
    H.span ! class_ "fa-stack fa-1x" ! A.style "font-size: 0.5em; height: 2.6em" $ do
        i ! class_ ("fas fa-stack-1x fa-" <> iconName) $ mempty
        i ! class_ "fas fa-ban fa-stack-2x" $ mempty