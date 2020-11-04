{-# LANGUAGE OverloadedStrings #-}

module Blog.Feed where

import           Blog.Types
import           Data.Text  (Text)

makeRSSFeed :: [BlogPost] -> Text
makeRSSFeed _ = ""
