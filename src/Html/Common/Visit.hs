{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE UnicodeSyntax     #-}

module Html.Common.Visit where

import Data.Env
import Control.Monad.Trans.Reader
import           Text.Blaze.Html5
import           Text.Blaze.Html5.Attributes as A

visitsUrl ∷ WebsiteIO AttributeValue
visitsUrl = do
    endpoint' <- asks endpoint
    pure . textValue $ endpoint' <> "/v.gif"

-- TODO functor?
lazyPixelVisit :: (AttributeValue -> AttributeValue) -> WebsiteIO Html
lazyPixelVisit f = do
    visitsUrl' <- visitsUrl
    pure $ img ! src (f visitsUrl') ! A.style "display: none" ! A.style "width: 0; height: 0" ! customAttribute "loading" "lazy"

visit ∷ AttributeValue → WebsiteIO Html
visit url' = lazyPixelVisit (<> "?u=" <> url')

visitPage ∷ AttributeValue → AttributeValue → WebsiteIO Html
visitPage url' page = lazyPixelVisit (<> "?u=" <> url' <> "&p=" <> page)

visitPageSub ∷ AttributeValue → AttributeValue → AttributeValue → WebsiteIO Html
visitPageSub url' page sub' = lazyPixelVisit (<> "?u=" <> url' <> "&p=" <> page <> "&s=" <> sub')