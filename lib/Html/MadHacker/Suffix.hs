{-# LANGUAGE OverloadedStrings #-}

module Html.MadHacker.Suffix where

import Control.Monad               (replicateM_)
import Data.Foldable
import Data.String                 (IsString (fromString))
import Data.Text                   qualified as T
import Html.Common.Blog.Types
import Text.Blaze.Html5
import Text.Blaze.Html5            qualified as H
import Text.Blaze.Html5.Attributes as A

-- Looks like this isn't being included anywhere... todo include it!

stars ∷ Score → Html
stars (Score value' total) = do
    H.span ! A.class_ "star-full" $ replicateM_ value' ((i ! class_ "fas fa-star") mempty)
    H.span ! A.class_ "star-empty" $ replicateM_ (total - value') ((i ! class_ "far fa-star") mempty)

renderStars ∷ BlogMetadata → Html
renderStars BlogMetadata { scores = Just scores' } = do
    h3 "Overall Scores"
    br
    traverse_ (\(name', score) -> do
       H.div ! class_ "row my-sm-2 my-xs-2 my-lg-1" $ do
            ((H.div ! class_ "col-lg-4 col-md-4 col-sm-12") . h4) . fromString . T.unpack $ name'
            H.div ! class_ "col-lg-4 col-md-4 col-sm-12" $ stars score
            (H.div ! class_ "col-lg-4 col-md-4 col-sm-12") . h5 $ do
                fromString (show (rating score))
                "/"
                fromString (show (outOf score))
        ) scores'
renderStars _ = mempty
