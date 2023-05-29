{-# LANGUAGE OverloadedStrings #-}

module Html.Common.Blog.Link where

import qualified Data.List                   as L
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

makeLink :: BlogPost -> Html
makeLink link' = do
    p ! class_ "ps-2" $ renderLink link'
    br

genericMakeLinks :: Foldable t => (t a -> String) -> (a -> Html) -> t a -> Html
genericMakeLinks formatter makeSubLinks byPeriod = details ! customAttribute "open" "" ! class_ "ps-2" $ do
     H.summary . fromString . formatter $ byPeriod
     p $ foldMap makeSubLinks byPeriod

makeLinksByMonth :: [BlogPost] -> Html
makeLinksByMonth = genericMakeLinks (formatTime defaultTimeLocale "%B" . date . metadata . L.head) makeLink

makeLinksByYear :: [[BlogPost]] -> Html
makeLinksByYear = genericMakeLinks (show . year . date . metadata . L.head . L.head) makeLinksByMonth
         
makeLinks ∷ [BlogPost] → Html
makeLinks = foldMap (makeLinksByYear . groupOn (month . date . metadata)) . groupOn (year . date . metadata)
