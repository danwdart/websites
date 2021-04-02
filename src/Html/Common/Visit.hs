{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE UnicodeSyntax     #-}

module Html.Common.Visit where

import Data.Env
import Control.Monad.Trans.Reader
import           Text.Blaze.Html5
import           Text.Blaze.Html5.Attributes as A

visitsUrl ∷ WebsiteM AttributeValue
visitsUrl = do
    endpoint' <- asks endpoint
    pure . textValue $ endpoint' <> "/v.gif"

-- TODO functor?
lazyPixelVisit :: (AttributeValue -> AttributeValue) -> WebsiteM Html
lazyPixelVisit f = do
    visitsUrl' <- visitsUrl
    pure $ img ! src (f visitsUrl') ! A.style "display: none" ! A.style "width: 0; height: 0" ! customAttribute "loading" "lazy"

visit ∷ AttributeValue → WebsiteM Html
visit url' = lazyPixelVisit (<> "?u=" <> url')

visitPage ∷ AttributeValue → AttributeValue → WebsiteM Html
visitPage url' page = lazyPixelVisit (<> "?u=" <> url' <> "&p=" <> page)

visitPageSub ∷ AttributeValue → AttributeValue → AttributeValue → WebsiteM Html
visitPageSub url' page sub' = lazyPixelVisit (<> "?u=" <> url' <> "&p=" <> page <> "&s=" <> sub')