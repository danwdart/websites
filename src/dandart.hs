{-# LANGUAGE OverloadedStrings #-}

import Build.Utils
import qualified Data.ByteString.Lazy.Char8 as BSL
import Distribution.Simple.Utils
import Html.DanDart.Index
import System.Path
import Text.Blaze.Html.Renderer.Utf8

main :: IO ()
main = do
    -- Copy all static files
    copyDir "static/common" ".sites/dandart"
    copyDir "static/dandart" ".sites/dandart"
    -- Build HTML
    BSL.writeFile ".sites/dandart/index.html" $ renderHtml page
    -- Build CSS?
    -- Deploy?
    putStrLn "dandart compiled."