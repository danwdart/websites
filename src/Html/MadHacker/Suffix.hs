{-# LANGUAGE OverloadedLists   #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE UnicodeSyntax     #-}

module Html.MadHacker.Suffix (renderStars) where

import           Blog.Types                  (BlogMetadata (BlogMetadata))
import           Control.Monad               (replicateM_)
import           Data.Aeson
import           Data.String                 (IsString (fromString))
import           Data.Text                   (Text)
import qualified Data.Text                   as T
import           Text.Blaze.Html5
import qualified Text.Blaze.Html5            as H
import           Text.Blaze.Html5.Attributes as A
import Html.Common.Bootstrap

stars ∷ Text → Html
stars score = do
    let [value', total] = read . T.unpack <$> T.split (== '/') score
    H.span ! A.style "color: gold" $ replicateM_ value' ((i ! class_ "fas fa-star") mempty)
    H.span ! A.style "color: black" $ replicateM_ (total - value') ((i ! class_ "far fa-star") mempty)

renderStars ∷ BlogMetadata → Html
renderStars (BlogMetadata _ _ _ _ _ (Just (Array scores))) = do
    h3 "Overall Scores"
    br
    mapM_ (\(Object [(name', String score)]) -> do
        row $ do
            (H.div ! class_ "col") . h4 $ (fromString . T.unpack $ name')
            H.div ! class_ "col" $ stars score
        ) scores
renderStars _ = mempty
