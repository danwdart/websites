{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-x-partial #-}

module Html.Common.Blog.Link where

import Data.List.NonEmpty          (NonEmpty)
import Data.List.NonEmpty          qualified as LNE
import Data.String
import Data.Text                   (Text)
import Data.Text                   qualified as T
import Data.Time
import Data.Time.Utils
import Html.Common.Blog.Types      as BT
import Text.Blaze.Html5            as H hiding (main)
import Text.Blaze.Html5.Attributes as A

renderMetaLink ∷ Text → BlogMetadata → Html
renderMetaLink postId' m = a ! href (fromString ("/#" <> T.unpack postId')) $ fromString (T.unpack (BT.title m))

renderLink ∷ BlogPost → Html
renderLink bp = renderMetaLink (postId bp) (metadata bp)

makeLink ∷ BlogPost → Html
makeLink link' = do
    p ! class_ "ps-2" $ renderLink link'
    br

genericMakeLinks ∷ Foldable t ⇒ (t anyLink → String) → (anyLink → Html) → t anyLink → Html
genericMakeLinks formatter makeSubLinks byPeriod = details ! customAttribute "open" "" ! class_ "ps-2" $ do
     H.summary . fromString . formatter $ byPeriod
     p $ foldMap makeSubLinks byPeriod

makeLinksByMonth ∷ NonEmpty BlogPost → Html
makeLinksByMonth = genericMakeLinks (formatTime defaultTimeLocale "%B" . date . metadata . LNE.head) makeLink -- you could use comonad extract here but what is a type with a head

makeLinksByYear ∷ NonEmpty (NonEmpty BlogPost) → Html
makeLinksByYear = genericMakeLinks (show . year . date . metadata . LNE.head . LNE.head) makeLinksByMonth

-- TODO libify
groupOnNonEmpty ∷ Eq k ⇒ (a' → k) → NonEmpty a' → NonEmpty (NonEmpty a')
groupOnNonEmpty f = LNE.groupBy1 ((==) `on2` f)
    -- redefine on so we avoid duplicate computation for most values.
    where (.*.) `on2` fn = \x -> let fx = fn x in \y -> fx .*. fn y

makeLinks ∷ NonEmpty BlogPost → Html
makeLinks = foldMap (makeLinksByYear . groupOnNonEmpty (month . date . metadata)) . groupOnNonEmpty (year . date . metadata)
