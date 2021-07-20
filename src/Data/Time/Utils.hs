{-# LANGUAGE UnicodeSyntax #-}
module Data.Time.Utils where

import           Control.Applicative      ((<|>))
import           Data.Maybe               (fromJust)
import           Data.Time                (UTCTime, ZonedTime, toGregorian,
                                           utctDay, zonedTimeToUTC)
import           Data.Time.Format.ISO8601 (iso8601ParseM)
import           Data.Tuple.Triple        (t1, t2)

stringToTime ∷ String → UTCTime
stringToTime s = fromJust (
    (zonedTimeToUTC <$> (iso8601ParseM s :: Maybe ZonedTime)) <|>
    iso8601ParseM s
    )

year ∷ UTCTime → Integer
year = t1 . toGregorian . utctDay

month ∷ UTCTime → Int
month = t2 . toGregorian . utctDay
