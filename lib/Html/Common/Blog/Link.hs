{-# LANGUAGE OverloadedStrings #-}

module Html.Common.Blog.Link where

import           Data.List                   as L
import           Data.List.Extra             as LE
import           Data.String
import           Data.Text                   (Text)
import qualified Data.Text                   as T
import           Data.Time
import           Data.Time.Utils
import           Html.Common.Blog.Types      as BT
import           Text.Blaze.Html5            as H hiding (main)
import           Text.Blaze.Html5.Attributes as A

renderMetaLink ∷ Text → BlogMetadata → Html
renderMetaLink postId' m = a ! href (fromString ("#" <> T.unpack postId')) $ fromString (T.unpack (BT.title m))

renderLink ∷ BlogPost → Html
renderLink bp = renderMetaLink (postId bp) (metadata bp)

makeLinks ∷ [BlogPost] → Html
makeLinks = foldMap ((
        \byYear -> do
            details ! customAttribute "open" "" ! class_ "ps-2" $ do
                H.summary . fromString . show . year . date . metadata . L.head . L.head $ byYear
                p $ foldMap (
                    \byMonth -> details! customAttribute "open" "" ! class_ "ps-2" $ do
                        -- "%B" is Month
                        H.summary . fromString . formatTime defaultTimeLocale "%B" . date . metadata . L.head $ byMonth
                        p $ foldMap (\link' -> do
                            p ! class_ "ps-2" $ renderLink link'
                            br
                            ) byMonth
                    ) byYear
        ) .
        groupOn (month . date . metadata)
    ) . groupOn (year . date . metadata)
