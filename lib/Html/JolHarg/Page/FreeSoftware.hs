{-# LANGUAGE OverloadedStrings #-}

module Html.JolHarg.Page.FreeSoftware where

import Control.Monad.Reader
import Data.Env.Types
import Html.Common.Bootstrap
import Html.Common.Card
import Html.Common.GitHub
import Html.Common.Page
import Text.Blaze.Html5            as H hiding (main)
import Text.Blaze.Html5.Attributes as A

pageFs ∷ (MonadReader [Repo] n, MonadReader Website m) ⇒ n (m Html)
pageFs = do
    repos <- ask
    pure . makePage "fs" "Free Software" customLayout notDefaultPage $ do
        row . (H.div ! class_ "col-md-12 text-center") $ p "Some of the free software projects JolHarg Ltd has created or contributed to are:"
        mapM_ renderCard repos
