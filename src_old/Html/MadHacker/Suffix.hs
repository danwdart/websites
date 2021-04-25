{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE UnicodeSyntax     #-}

module Html.MadHacker.Suffix (renderStars) where

import           Blog.Types
import           Control.Monad               (replicateM_)
import           Data.String                 (IsString (fromString))
import qualified Data.Text                   as T
import           Html.Common.Bootstrap
import           Text.Blaze.Html5 (Html, (!))
import qualified Text.Blaze.Html5            as H
import           Text.Blaze.Html5.Attributes as A

stars ∷ Score → Html
stars (Score value' total) = do
    H.span ! A.style "color: gold" $ replicateM_ value' ((H.i ! A.class_ "fas fa-star") mempty)
    H.span ! A.style "color: black" $ replicateM_ (total - value') ((H.i ! A.class_ "far fa-star") mempty)

renderStars ∷ BlogMetadata → Html
renderStars BlogMetadata { scores = Just scores' } = do
    H.h3 "Overall Scores"
    H.br
    mapM_ (\(name', score) -> do
        row $ do
            (col . H.h4) . fromString . T.unpack $ name'
            col $ stars score
        ) scores'
renderStars _ = mempty
