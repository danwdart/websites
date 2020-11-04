{-# LANGUAGE OverloadedStrings #-}
module Blog.Link where

import           Blog.Post                   (getPostId)
import           Blog.Types
import           Data.List
import           Data.String
import qualified Data.Text                   as T
import           Data.Time
import           Text.Blaze.Html5            as H hiding (main)
import           Text.Blaze.Html5.Attributes as A
import           Util.List
import           Util.Time

renderMetaLink :: BlogMetadata -> Html
renderMetaLink m = a ! href (fromString ("#" <> getPostId m)) $ fromString (T.unpack (Blog.Types.title m))

renderLink :: BlogPost -> Html
renderLink = renderMetaLink . metadata

makeLinks :: [BlogPost] -> Html
makeLinks = foldMap ((
        \byYear -> do
            details ! customAttribute "open" "" ! class_ "pl-2" $ do
                H.summary $ fromString . show . year . date . metadata . Data.List.head . Data.List.head $ byYear
                p $ foldMap (
                    \byMonth -> details! customAttribute "open" "" ! class_ "pl-2" $ do
                        -- "%B" is Month
                        H.summary $ fromString . formatTime defaultTimeLocale "%B" . date . metadata . Data.List.head $ byMonth
                        p $ foldMap (\link' -> do
                            p ! class_ "pl-2" $ renderLink link'
                            br
                            ) byMonth
                    ) byYear
        ) .
        groupOn (month . date . metadata)
    ) . groupOn (year . date . metadata)
