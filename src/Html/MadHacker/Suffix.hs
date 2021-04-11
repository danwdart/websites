{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE UnicodeSyntax     #-}

module Html.MadHacker.Suffix (renderStars) where

import           Blog.Types
import           Control.Monad               (replicateM_)
import           Data.String                 (IsString (fromString))
import qualified Data.Text                   as T
import           Html.Common.Bootstrap
import           Text.Blaze.Html5
import qualified Text.Blaze.Html5            as H
import           Text.Blaze.Html5.Attributes as A

stars ∷ Score → Html
stars (Score value' total) = do
    H.span ! A.style "color: gold" $ replicateM_ value' ((i ! class_ "fas fa-star") mempty)
    H.span ! A.style "color: black" $ replicateM_ (total - value') ((i ! class_ "far fa-star") mempty)

renderStars ∷ BlogMetadata → Html
renderStars BlogMetadata { scores = Just scores' } = do
    h3 "Overall Scores"
    br
    mapM_ (\(name', score) -> do
        row $ do
            ((H.div ! class_ "col") . h4) . fromString . T.unpack $ name'
            H.div ! class_ "col" $ stars score
        ) scores'
renderStars _ = mempty
