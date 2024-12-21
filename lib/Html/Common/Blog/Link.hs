{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-x-partial #-}

module Html.Common.Blog.Link where

import Data.List.NonEmpty          (NonEmpty)
import Data.List.NonEmpty          qualified as LNE
import Data.Map                    (Map)
import Data.Map                    qualified as M
-- import Data.Map.NonEmpty           (NEMap)
-- import Data.Map.NonEmpty           qualified as MNE
import Data.String
import Data.Text                   (Text)
import Data.Text                   qualified as T
import Data.Time
import Data.Time.Utils
import Html.Common.Blog.Types      as BT
import Text.Blaze.Html5            as H hiding (main)
import Text.Blaze.Html5.Attributes as A

-- TODO make this choose between /# and #
renderMetaLink ∷ String -> Text → BlogMetadata → Html
renderMetaLink preLink postId' m = a ! href (fromString (preLink <> T.unpack postId')) $ fromString (T.unpack (BT.title m))

renderLink ∷ String -> BlogPost → Html
renderLink preLink bp = renderMetaLink preLink (postId bp) (metadata bp)

makeLink ∷ String -> BlogPost → Html
makeLink preLink link' = do
    p ! class_ "ps-2" $ renderLink preLink link'
    br

genericMakeLinks ∷ Foldable t ⇒ Bool → (t anyLink → String) → (anyLink → Html) → t anyLink → Html
genericMakeLinks opened formatter makeSubLinks byPeriod = do
    (if opened
        then details ! customAttribute "open" "" ! class_ "ps-2"
        else details ! class_ "ps-2") $ do
            H.summary . fromString . formatter $ byPeriod
            p $ foldMap makeSubLinks byPeriod

makeLinksByMonth ∷ String -> Bool → NonEmpty BlogPost → Html
makeLinksByMonth preLink opened = genericMakeLinks opened (formatTime defaultTimeLocale "%B" . date . metadata . LNE.head) (makeLink preLink) -- you could use comonad extract here but what is a type with a head

makeLinksByYear ∷ String -> Bool → NonEmpty (NonEmpty BlogPost) → Html
makeLinksByYear preLink opened = genericMakeLinks opened (show . year . date . metadata . LNE.head . LNE.head) (makeLinksByMonth preLink opened)

-- why don't we make this an ordered map???
-- TODO libify
groupOnNonEmpty ∷ Eq k ⇒ (a' → k) → NonEmpty a' → NonEmpty (NonEmpty a')
groupOnNonEmpty f = LNE.groupBy1 ((==) `on2` f)
    -- redefine on so we avoid duplicate computation for most values.
    where (.*.) `on2` fn = \x -> let fx = fn x in \y -> fx .*. fn y

-- TODO convert to foldMap for NEMap
-- not doable with M.insertMapWith because it's within a fold (i.e. first insertMapWith, second insertWith)
-- unless we go crazy with pattern matching, better just to use foldMap1 or something
groupOnNonEmptyWithKey ∷ (Ord b') ⇒ (a' → b') → NonEmpty a' → Map b' (NonEmpty a')
groupOnNonEmptyWithKey f = foldr (\v acc -> M.insertWith (<>) (f v) (LNE.singleton v) acc) M.empty

-- TODO open only the links we're on if we're in a post page
makeLinks ∷ String -> Text -> NonEmpty BlogPost → Html
makeLinks preLink titleName bps = do
    (H.div ! class_ "d-none d-lg-block") $ do
        (details ! customAttribute "open" "" ! class_ "ps-2") $ do
            H.summary . text $ titleName
            foldMap (makeLinksByYear preLink True . groupOnNonEmpty (month . date . metadata)) . groupOnNonEmpty (year . date . metadata) $ bps
        {-(details ! customAttribute "open" "" ! class_ "ps-2") $ do
            H.summary "Tags"
            foldMap (\tag -> do
                p . (a ! href (fromString $ "/tag" <> T.unpack (getTag tag))) $ fromString (T.unpack (getTag tag))
                ) tags -}
    (H.div ! class_ "d-lg-none") $ do
        (details ! class_ "ps-2") $ do
            H.summary . text $ titleName
            foldMap (makeLinksByYear preLink False . groupOnNonEmpty (month . date . metadata)) . groupOnNonEmpty (year . date . metadata) $ bps
        (details ! class_ "ps-2") $
            H.summary "Tags"

-- TODO open only the letters we're in if we're in a tag page
makeTags ∷ NonEmpty BlogTag → Html
makeTags tags = do
    (H.div ! class_ "d-none d-lg-block") $ do
        (details ! customAttribute "open" "" ! class_ "ps-2") $ do
            H.summary "Tags"
            innerElement
    (H.div ! class_ "d-lg-none") $ do
        (details ! customAttribute "open" "" ! class_ "ps-2") $ do
            H.summary "Tags"
            innerElement
    where
        sortedTags = groupOnNonEmptyWithKey (T.toLower . T.singleton . T.head . getTag) tags :: Map Text (NonEmpty BlogTag)
        innerElement = ul $
            (M.foldMapWithKey :: (Text → NonEmpty BlogTag → Html) → Map Text (NonEmpty BlogTag) → Html)  (\letter subtags ->
                li . (details ! class_ "ps-2") $ do
                    H.summary . fromString . T.unpack $ letter
                    ul $ foldMap (\tag ->
                        li $ do
                            (a ! href (fromString $ "/tag/" <> T.unpack (getTag tag))) $ fromString (T.unpack (getTag tag))
                            -- " "
                            -- (a ! href (fromString $ "/tag/" <> T.unpack (getTag tag) <> "/atom.xml")) "📰"
                        ) subtags
            ) (sortedTags :: Map Text (NonEmpty BlogTag))
          {-foldMap (\tag -> do
                li . (a ! href (fromString $ "/tag/" <> T.unpack (getTag tag))) $ fromString (T.unpack (getTag tag))
                ) tags -}
