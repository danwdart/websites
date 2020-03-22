{-# LANGUAGE OverloadedStrings #-}

import Build.Utils
import qualified Data.ByteString.Lazy.Char8 as BSL
import Distribution.Simple.Utils
import Html.M0ORI.Index
import System.Path
import Text.Blaze.Html.Renderer.Utf8

main :: IO ()
main = do
    -- Copy all static files
    copyDir "static/common" ".sites/m0ori"
    copyDir "static/m0ori" ".sites/m0ori"
    -- Build HTML
    BSL.writeFile ".sites/m0ori/index.html" $ renderHtml page
    -- Build CSS?
    -- Deploy?
    putStrLn "m0ori compiled."