{-# LANGUAGE OverloadedStrings #-}

module Html.DanDart.Page.VA where

import Control.Monad.Reader
import Data.Env.Types
import Data.Foldable
import Data.NonEmpty               qualified as NE
import Data.String
import Html.Common.Link
import Html.Common.Page
import Html.Common.Shortcuts
import Html.DanDart.Data
import Text.Blaze.Html5      as H hiding (main)

pageTalks ∷ (MonadReader Website m) ⇒ m Html
pageTalks = plainBreadcrumb (NE.trustedNonEmpty "Voice Acting") . makePage "va" "Voice Acting" defaultLayout notDefaultPage $ do
    p "I am available to hire for voice acting on a remote basis. Examples of my range are coming soon."