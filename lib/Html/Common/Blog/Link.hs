{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-x-partial #-}

module Html.Common.Blog.Link where

import Data.Char
import Data.Foldable
import Data.List.NonEmpty          (NonEmpty (..))
import Data.List.NonEmpty          qualified as LNE
-- import Data.Map                    (Map)
-- import Data.Map                    qualified as M
import Data.Map.NonEmpty           (NEMap)
import Data.Map.NonEmpty           qualified as MNE
import Data.NonEmpty               qualified as NE
import Data.Text                   qualified as T
import Data.Text.NonEmpty          (NonEmptyText)
import Data.Text.NonEmpty          qualified as TNE
import Data.Time
import Data.Time.Utils
import Html.Common.Blog.Types      as BT
import Text.Blaze.Html5            as H hiding (main)
import Text.Blaze.Html5.Attributes as A

data DetailsOpenOrClosed = Closed | Open

detailsEl ∷ DetailsOpenOrClosed → Html → Html
detailsEl Open   = details ! customAttribute "open" "" ! class_ "ps-2"
detailsEl Closed = details ! class_ "ps-2"

detailsOp ∷ Bool → Html → Html
detailsOp True  = detailsEl Open
detailsOp False = detailsEl Closed

-- TODO make this choose between /# and #
renderMetaLink ∷ Maybe NonEmptyText → NonEmptyText → NonEmptyText → BlogMetadata → Html
renderMetaLink mCurrId preLink postId' m =
    if Just postId' == mCurrId
    then em . strong $ text (NE.getNonEmpty $ BT.title m)
    else a ! href (textValue $ NE.getNonEmpty (preLink <> postId')) $ text . NE.getNonEmpty $ (BT.title m)

renderLink ∷ Maybe NonEmptyText → NonEmptyText → BlogPost → Html
renderLink mCurrId preLink bp = renderMetaLink mCurrId preLink (postId bp) (metadata bp)

makeLink ∷ Maybe NonEmptyText → NonEmptyText → BlogPost → Html
makeLink mCurrId preLink link' = do
    p ! class_ "ps-2" $ renderLink mCurrId preLink link'
    br

genericMakeLinks ∷ Foldable t ⇒ Bool → (t anyLink → NonEmptyText) → (anyLink → Html) → t anyLink → Html
genericMakeLinks opened formatter makeSubLinks byPeriod = do
    detailsOp opened $ do
        H.summary . text . NE.getNonEmpty . formatter $ byPeriod
        p $ foldMap' makeSubLinks byPeriod

makeLinksByMonth ∷ Maybe NonEmptyText → NonEmptyText → Bool → NonEmpty BlogPost → Html
makeLinksByMonth mCurrId preLink opened = genericMakeLinks opened (NE.trustedNonEmpty . T.pack . formatTime defaultTimeLocale "%B" . date . metadata . LNE.head) (makeLink mCurrId preLink) -- you could use comonad extract here but what is a type with a head

makeLinksByYear ∷ Maybe NonEmptyText → NonEmptyText → Bool → NonEmpty (NonEmpty BlogPost) → Html
makeLinksByYear mCurrId preLink opened = genericMakeLinks opened (NE.trustedNonEmpty . T.show . year . date . metadata . LNE.head . LNE.head) (makeLinksByMonth mCurrId preLink opened)

-- why don't we make this an ordered map???
-- TODO libify
groupOnNonEmpty ∷ Eq k ⇒ (a' → k) → NonEmpty a' → NonEmpty (NonEmpty a')
groupOnNonEmpty f = LNE.groupBy1 ((==) `on2` f)
    -- redefine on so we avoid duplicate computation for most values.
    where (.*.) `on2` fn = \x -> let fx = fn x in \y -> fx .*. fn y

-- TODO convert to foldMap' for NEMap
-- not doable with M.insertMapWith because it's within a fold (i.e. first insertMapWith, second insertWith)
-- unless we go crazy with pattern matching, better just to use foldMap1 or something
-- groupOnNonEmptyWithKey ∷ (Ord b') ⇒ (a' → b') → NonEmpty a' → Map b' (NonEmpty a')
-- groupOnNonEmptyWithKey f = foldr (\v acc -> M.insertWith (<>) (f v) (LNE.singleton v) acc) M.empty

-- thanks chatgpt
groupOnNonEmptyWithKey ∷ (Ord b') ⇒ (a' → b') → NonEmpty a' → NEMap b' (NonEmpty a')
groupOnNonEmptyWithKey f = MNE.fromListWith (<>) . fmap (\x -> (f x, x :| []))

-- TODO open only the links we're on if we're in a post page
makeLinks ∷ Maybe NonEmptyText → NonEmptyText → NonEmptyText → NonEmpty BlogPost → Html
makeLinks mCurrId preLink titleName bps = do
    (H.div ! class_ "d-none d-lg-block") $ do
        detailsEl Open $ do
            H.summary . text . NE.getNonEmpty $ titleName
            foldMap' (makeLinksByYear mCurrId preLink True . groupOnNonEmpty (month . date . metadata)) . groupOnNonEmpty (year . date . metadata) $ bps
    (H.div ! class_ "d-lg-none") $ do
        detailsEl Closed $ do
            H.summary . text . NE.getNonEmpty $ titleName
            foldMap' (makeLinksByYear mCurrId preLink False . groupOnNonEmpty (month . date . metadata)) . groupOnNonEmpty (year . date . metadata) $ bps

-- TODO open only the letters we're in if we're in a tag page
makeTags ∷ Maybe BlogTag → NonEmpty BlogTag → Html
makeTags mCurrTag tags = do
    (H.div ! class_ "d-none d-lg-block") $ do
        detailsEl Open $ do
            H.summary "Tags"
            innerElement
    (H.div ! class_ "d-lg-none") $ do
        detailsEl Closed $ do
            H.summary "Tags"
            innerElement
    where
        sortedTags = groupOnNonEmptyWithKey (toLower . TNE.head . getTag) tags :: NEMap Char (NonEmpty BlogTag)
        innerElement = ul $
            (MNE.foldMapWithKey :: (Char → NonEmpty BlogTag → Html) → NEMap Char (NonEmpty BlogTag) → Html)  (\letter subtags ->
                li . detailsOp (any (`elem` subtags) mCurrTag) $ do
                    H.summary . text . T.singleton $ letter
                    ul $ foldMap' (\tag ->
                        li $ do
                            if mCurrTag == Just tag
                            then em . strong $ text (NE.getNonEmpty (getTag tag))
                            else (a ! rel "tag" ! href (textValue $ "/tag/" <> NE.getNonEmpty (getTag tag))) $ text (NE.getNonEmpty (getTag tag))
                            -- " "
                            -- (a ! href (fromString $ "/tag/" <> T.unpack (getTag tag) <> "/atom.xml")) "📰"
                        ) subtags
            ) (sortedTags :: NEMap Char (NonEmpty BlogTag))
          {-foldMap' (\tag -> do
                li . (a ! href (fromString $ "/tag/" <> T.unpack (getTag tag))) $ fromString (T.unpack (getTag tag))
                ) tags -}
